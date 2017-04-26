open Base
open Stdio
open Cmdliner
open Sodium

open Libbitcoin

open Util
open Util.Cmdliner

let tezos_pkh_size = 20

type loglevel = [`Error | `Info | `Debug]
let loglevel = ref (`Error :> loglevel)

module Wallet = struct
  type t = {
    mnemonic : string list ;
    pkh : Hex.t ;
    addr : Base58.t ;
  }

  let encoding =
    let open Json_encoding in
    conv
      (fun { mnemonic ; pkh = `Hex pkh_hex ; addr = `Base58 addr_base58 } ->
         (mnemonic, pkh_hex, addr_base58))
      (fun (mnemonic, pkh, addr) ->
         { mnemonic ; pkh = `Hex pkh ;
           addr = Base58.of_string_exn addr })
      (obj3
         (req "mnemonic" (list string))
         (req "pkh" string)
         (req "addr" string))

  let to_ezjsonm wallet = Json_encoding.construct encoding wallet

  let pp ppf { mnemonic ; pkh = `Hex pkh_str ; addr } =
    let open Caml.Format in
    let pp_mnemonic =
      pp_print_list
        ~pp_sep:(fun fmt () -> fprintf fmt " ")
        pp_print_string in
    fprintf ppf "%a@.%s@.%a"
      pp_mnemonic mnemonic pkh_str Base58.pp addr

  let show t =
    Caml.Format.asprintf "%a" pp t
end

let generate_seed cfg version seed_bytes =
  let seed = Sign.Bytes.to_seed seed_bytes in
  let sk, pk = Sign.seed_keypair seed in
  let pk_bytes = Sign.Bytes.of_public_key pk in
  let h = Generichash.init ~size:tezos_pkh_size () in
  Generichash.Bytes.update h pk_bytes ;
  let pkh = Generichash.final h in
  let pkh_bytes = Generichash.Bytes.of_hash pkh in
  let script = Script.P2SH_multisig.redeem
      ~append_data:pkh_bytes ~threshold:cfg.Cfg.threshold cfg.pks in
  let `Hex script_hex = Hex.of_string (Script.to_bytes script) in
  begin match !loglevel with
  | `Debug -> Stdio.eprintf "%s\n" script_hex ;
  | #loglevel -> ()
  end ;
  match Payment_address.of_script ~version script with
  | None -> failwith "generate_seed"
  | Some addr ->
    (Hex.of_string pkh_bytes),
    Payment_address.to_b58check addr

let generate_one cfg version passphrase =
  let entropy = Random.Bytes.generate 20 in
  let mnemonic = Mnemonic.of_entropy entropy in
  let seed_bytes = String.subo ~len:32
      (Option.value_exn (Mnemonic.to_seed mnemonic ~passphrase)) in
  let pkh, addr = generate_seed cfg version seed_bytes in
  Wallet.{ mnemonic ; pkh ; addr }

let generate_n cfg version passphrase n =
  let rec inner acc n =
    if n > 0 then
      inner ((generate_one cfg version passphrase) :: acc) (Caml.pred n)
    else acc
  in inner [] n

let generate cfg ll testnet json_out n =
  begin match List.length ll with
    | 1 -> loglevel := `Info
    | 2 -> loglevel := `Debug
    | _ -> loglevel := `Error
  end ;
  let version =
    Payment_address.(if testnet then Testnet_P2SH else P2SH) in
  match getpass_confirm () with
  | None ->
      prerr_endline "Passphrase do not match. Aborting." ;
      Caml.exit 1
  | Some passphrase ->
      Random.stir () ;
      let wallets = generate_n cfg version passphrase n in
      if json_out then
        let ret = Ezjsonm.to_string (`A (List.map ~f:Wallet.to_ezjsonm wallets)) in
        Out_channel.printf "%s\n" ret
      else
        Caml.Format.(printf "%a@." (pp_print_list Wallet.pp) wallets)

let check cfg testnet mnemonic =
  let version =
    Payment_address.(if testnet then Testnet_P2SH else P2SH) in
  let passphrase = getpass () in
  match Mnemonic.to_seed ~passphrase mnemonic with
  | None ->
      prerr_endline "Provided mnemonic is invalid" ;
      Caml.exit 1
  | Some seed_bytes ->
    let pkh, addr = generate_seed cfg version (String.subo ~len:32 seed_bytes) in
    Out_channel.printf "%s\n" Wallet.(show { mnemonic ; pkh ; addr })

let generate =
  let doc = "Generate a Tezos wallet." in
  let n = Arg.(value & (pos 0 int 1) & info [] ~docv:"N") in
  Term.(const generate $ cfg $ Cmdliner.loglevel $ testnet $ json $ n),
  Term.info ~doc "generate"

let check =
  let doc = "Check a Tezos wallet." in
  let words =
    Arg.(value & (pos_all string []) & info [] ~docv:"WORDS") in
  Term.(const check $ cfg $ testnet $ words),
  Term.info ~doc "check"

let default_cmd =
  let doc = "Wallet operations." in
  Term.(ret (const (`Help (`Pager, None)))),
  Term.info ~doc "wallet"

let cmds = [ generate ; check ]

let () = match Term.eval_choice default_cmd cmds with
  | `Error _ -> Caml.exit 1
  | #Term.result -> Caml.exit 0
