open Base
open Stdio
open Cmdliner
open Tweetnacl
open Blake2

open Util
open Util.Cmdliner

let c = Bitcoin.Util.c

let tezos_pkh_size = 20

type loglevel = [`Error | `Info | `Debug]
let loglevel = ref (`Error : loglevel)

let ctx = Util.ctx

module Wallet = struct
  type t = {
    mnemonic : Bip39.t ;
    tezos_addr : Base58.Tezos.t ;
    payment_addr : Base58.Bitcoin.t ;
  }

  let encoding =
    let open Json_encoding in
    conv
      (fun { mnemonic ; tezos_addr ; payment_addr } ->
         Base58.(Bip39.to_words mnemonic,
                 Tezos.to_string c tezos_addr,
                 Bitcoin.to_string c payment_addr))
      (fun (mnemonic, tezos_addr, payment_addr) ->
         let mnemonic = Bip39.of_words_exn mnemonic in
         let tezos_addr = Base58.Tezos.of_string_exn c tezos_addr in
         let payment_addr = Base58.Bitcoin.of_string_exn c payment_addr in
         { mnemonic ; tezos_addr ; payment_addr })
      (obj3
         (req "mnemonic" (list string))
         (req "tezos_pkh" string)
         (req "payment_addr" string))

  let to_ezjsonm wallet = Json_encoding.construct encoding wallet

  let pp ppf { mnemonic ; tezos_addr ; payment_addr } =
    let open Caml.Format in
    fprintf ppf "%a@.%a@.%a"
      Bip39.pp mnemonic
      (Base58.Tezos.pp c) tezos_addr
      (Base58.Bitcoin.pp c) payment_addr

  let show t =
    Caml.Format.asprintf "%a" pp t
end

let generate_seed ?(testnet=false) seed =
  let pk, sk = Sign.keypair ~seed () in
  let Blake2b.Hash pkh = Blake2b.direct (Sign.to_cstruct pk) tezos_pkh_size in
  let pkh_bytes = Cstruct.to_string pkh in
  let script = Util.multisig_script pkh [pk1; pk2] in
  (* let cs = Cstruct.create 1024 in *)
  (* let cs' = Bitcoin.Script.to_cstruct cs script in *)
  (* let `Hex script_hex = Hex.of_cstruct (Cstruct.sub cs 0 cs'.off) in
   * begin match !loglevel with
   * | `Debug -> Stdio.eprintf "%s\n" script_hex ;
   * | #loglevel -> ()
   * end ; *)
  Base58.Tezos.create ~version:Address ~payload:pkh_bytes,
  Bitcoin.Wallet.Address.of_script ~testnet script

let generate_one ?(testnet=false) passphrase =
  let entropy = Rand.gen 20 in
  let mnemonic = Bip39.of_entropy entropy in
  let seed = Bip39.to_seed mnemonic ~passphrase in
  let tezos_addr, payment_addr = generate_seed ~testnet seed in
  Wallet.{ mnemonic ; tezos_addr ; payment_addr }

let generate_n testnet passphrase n =
  let rec inner acc n =
    if n > 0 then
      inner ((generate_one ~testnet passphrase) :: acc) (Caml.pred n)
    else acc
  in inner [] n

let generate ll testnet json_out only_addrs n =
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
      let wallets = generate_n testnet passphrase n in
      match json_out, only_addrs with
      | true, _ ->
        let ret = Ezjsonm.to_string (`A (List.map ~f:Wallet.to_ezjsonm wallets)) in
        printf "%s\n" ret
      | _, true ->
        List.iter wallets
          ~f:(fun { tezos_addr } ->
              printf "%s\n" (Base58.Tezos.show c tezos_addr))
      | _ ->
        Caml.Format.(printf "%a@." (pp_print_list Wallet.pp) wallets)

let check testnet wordsfile =
  let mnemonic = match wordsfile with
    | None ->
      printf "Enter mnemonic: %!" ;
      In_channel.(input_line stdin)
    | Some fn -> List.hd (In_channel.read_lines fn) in
  let mnemonic = Option.map mnemonic ~f:(String.split ~on:' ') in
  match mnemonic with
  | Some words when List.length words = 15 -> begin
      let mnemonic = Bip39.of_words_exn words in
      printf "Enter wallet passphrase: %!" ;
      let passphrase = getpass () in
      let seed = Bip39.to_seed ~passphrase mnemonic in
      let tezos_addr, payment_addr = generate_seed ~testnet seed in
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
  Term.(const generate $ Cmdliner.loglevel $ testnet $ json $ only_addrs $ n),
  Term.info ~doc "generate"

let check =
  let doc = "Check a Tezos wallet." in
  let wordsfile =
    Arg.(value & (pos 0 (some file) None) & info [] ~docv:"WORDS") in
  Term.(const check $ testnet $ wordsfile),
  Term.info ~doc "check"

let payment_address testnet { Base58.Tezos.payload } =
  let script =
    Util.multisig_script (Cstruct.of_string payload) [pk1; pk2] in
  (* begin match !loglevel with
   * | `Debug -> Stdio.eprintf "%s\n" script_hex ;
   * | #loglevel -> ()
   * end ; *)
  let addr = Bitcoin.Wallet.Address.of_script ~testnet script in
  Caml.Format.printf "%a@." (Base58.Bitcoin.pp c) addr

let payment_address =
  let doc = "Get a payment address from a Tezos address." in
  let tezos_addr =
    Arg.(required & (pos 0 (some (Cmdliner.Conv.tezos_addr c)) None) & info [] ~docv:"TEZOS_ADDRESS") in
  Term.(const payment_address $ testnet $ tezos_addr),
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
