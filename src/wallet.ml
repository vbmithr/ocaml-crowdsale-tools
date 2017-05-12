open Base
open Stdio
open Cmdliner
open Sodium

open Libbitcoin

open Util
open Util.Cmdliner

let tezos_pkh_size = 20

type loglevel = [`Error | `Info | `Debug]
let loglevel = ref (`Error : loglevel)

module Wallet = struct
  type t = {
    mnemonic : string list ;
    tezos_addr : Base58.Tezos.t ;
    payment_addr : Base58.Bitcoin.t ;
  }

  let encoding =
    let open Json_encoding in
    conv
      (fun { mnemonic ; tezos_addr ; payment_addr } ->
         Base58.(mnemonic, Tezos.to_string tezos_addr, Bitcoin.to_string payment_addr))
      (fun (mnemonic, tezos_addr, payment_addr) ->
         let tezos_addr = Base58.Tezos.of_string_exn tezos_addr in
         let payment_addr = Base58.Bitcoin.of_string_exn payment_addr in
         { mnemonic ; tezos_addr ; payment_addr })
      (obj3
         (req "mnemonic" (list string))
         (req "tezos_pkh" string)
         (req "payment_addr" string))

  let to_ezjsonm wallet = Json_encoding.construct encoding wallet

  let pp ppf { mnemonic ; tezos_addr ; payment_addr } =
    let open Caml.Format in
    let pp_mnemonic =
      pp_print_list
        ~pp_sep:(fun fmt () -> fprintf fmt " ")
        pp_print_string in
    fprintf ppf "%a@.%a@.%a"
      pp_mnemonic mnemonic
      Base58.Tezos.pp tezos_addr
      Base58.Bitcoin.pp payment_addr

  let show t =
    Caml.Format.asprintf "%a" pp t
end

let generate_seed cfg seed_bytes =
  let seed = Sign.Bytes.to_seed seed_bytes in
  let sk, pk = Sign.seed_keypair seed in
  let pk_bytes = Sign.Bytes.of_public_key pk in
  let h = Generichash.init ~size:tezos_pkh_size () in
  Generichash.Bytes.update h pk_bytes ;
  let pkh = Generichash.final h in
  let pkh_bytes = Generichash.Bytes.of_hash pkh in
  let tezos_addr = Base58.Tezos.(create ~version:Address pkh_bytes) in
  let script =
    Script.P2SH_multisig.scriptRedeem
      ~append_script:Script.Script.Opcode.[Data pkh_bytes ; Drop]
      ~threshold:cfg.Cfg.threshold cfg.pks in
  let script = Script.of_script script in
  let `Hex script_hex = Hex.of_string (Script.to_bytes script) in
  begin match !loglevel with
  | `Debug -> Stdio.eprintf "%s\n" script_hex ;
  | #loglevel -> ()
  end ;
  let addr = Payment_address.of_script script in
  tezos_addr, Payment_address.to_b58check addr

let generate_one cfg passphrase =
  let entropy = Random.Bytes.generate 20 in
  let mnemonic = Mnemonic.of_entropy entropy in
  let seed_bytes =
    String.subo ~len:32 (Mnemonic.to_seed_exn mnemonic ~passphrase) in
  let tezos_addr, payment_addr = generate_seed cfg seed_bytes in
  Wallet.{ mnemonic ; tezos_addr ; payment_addr }

let generate_n cfg passphrase n =
  let rec inner acc n =
    if n > 0 then
      inner ((generate_one cfg passphrase) :: acc) (Caml.pred n)
    else acc
  in inner [] n

let generate cfg ll json_out only_addrs n =
  let cfg = Cfg.unopt cfg in
  begin match List.length ll with
    | 1 -> loglevel := `Info
    | 2 -> loglevel := `Debug
    | _ -> loglevel := `Error
  end ;
  match getpass_confirm () with
  | None ->
      prerr_endline "Passphrase do not match. Aborting." ;
      Caml.exit 1
  | Some passphrase ->
      Random.stir () ;
      let wallets = generate_n cfg passphrase n in
      match json_out, only_addrs with
      | true, _ ->
        let ret = Ezjsonm.to_string (`A (List.map ~f:Wallet.to_ezjsonm wallets)) in
        printf "%s\n" ret
      | _, true ->
        List.iter wallets
          ~f:(fun { tezos_addr } ->
              printf "%s\n" (Base58.Tezos.show tezos_addr))
      | _ ->
        Caml.Format.(printf "%a@." (pp_print_list Wallet.pp) wallets)

let check cfg wordsfile =
  let mnemonic = match wordsfile with
    | None ->
      eprintf "Enter mnemonic: %!" ;
      In_channel.(input_line stdin)
    | Some fn -> List.hd (In_channel.read_lines fn) in
  let mnemonic = Option.map mnemonic ~f:(String.split ~on:' ') in
  match mnemonic with
  | Some mnemonic when List.length mnemonic = 15 -> begin
    let cfg = Cfg.unopt cfg in
    let passphrase = getpass () in
    match Mnemonic.to_seed ~passphrase mnemonic with
    | None ->
      prerr_endline "Provided mnemonic is invalid." ;
      Caml.exit 1
    | Some seed_bytes ->
      let tezos_addr, payment_addr =
        generate_seed cfg (String.subo ~len:32 seed_bytes) in
      let wallet = { Wallet.mnemonic ; tezos_addr ; payment_addr } in
      printf "%s\n" Wallet.(show wallet)
  end
  | _ ->
    prerr_endline "Provided mnemonic must be 15 words." ;
    Caml.exit 1

let generate =
  let doc = "Generate a Tezos wallet." in
  let only_addrs =
    let doc = "Output only Tezos addresses." in
    Arg.(value & flag & info ["only-addrs"] ~doc) in
  let n = Arg.(value & (pos 0 int 1) & info [] ~docv:"N") in
  Term.(const generate $ cfg $ Cmdliner.loglevel $ json $ only_addrs $ n),
  Term.info ~doc "generate"

let check =
  let doc = "Check a Tezos wallet." in
  let wordsfile =
    Arg.(value & (pos 0 (some file) None) & info [] ~docv:"WORDS") in
  Term.(const check $ cfg $ wordsfile),
  Term.info ~doc "check"

let payment_address cfg { Base58.Tezos.payload } =
  let cfg = Cfg.unopt cfg in
  let script =
    Script.P2SH_multisig.scriptRedeem
      ~append_script:Script.Script.Opcode.[Data payload ; Drop]
      ~threshold:cfg.Cfg.threshold cfg.pks in
  let script = Script.of_script script in
  let `Hex script_hex = Hex.of_string (Script.to_bytes script) in
  begin match !loglevel with
  | `Debug -> Stdio.eprintf "%s\n" script_hex ;
  | #loglevel -> ()
  end ;
  let addr = Payment_address.of_script script in
  Caml.Format.printf "%a@." Payment_address.pp addr

let payment_address =
  let doc = "Get a payment address from a Tezos address." in
  let tezos_addr =
    Arg.(required & (pos 0 (some Cmdliner.Conv.tezos_addr) None) & info [] ~docv:"TEZOS_ADDRESS") in
  Term.(const payment_address $ cfg $ tezos_addr),
  Term.info ~doc "payment-address"

let default_cmd =
  let doc = "Wallet operations." in
  Term.(ret (const (`Help (`Pager, None)))),
  Term.info ~doc "wallet"

let cmds = [
  generate ;
  check ;
  payment_address ;
]

let () = match Term.eval_choice default_cmd cmds with
  | `Error _ -> Caml.exit 1
  | #Term.result -> Caml.exit 0
