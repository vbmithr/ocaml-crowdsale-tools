open Base
open Stdio
open Cmdliner
open Sodium

open Libbitcoin

open Util
open Util.Cmdliner

let tezos_pkh_size = 20

let generate_seed cfg version seed_bytes =
  let seed = Sign.Bytes.to_seed seed_bytes in
  let sk, pk = Sign.seed_keypair seed in
  let pk_bytes = Sign.Bytes.of_public_key pk in
  let h = Generichash.init ~size:tezos_pkh_size () in
  Generichash.Bytes.update h pk_bytes ;
  let pkh = Generichash.final h in
  let pkh_bytes = Generichash.Bytes.of_hash pkh in
  let `Hex pkh_hex = Hex.of_string pkh_bytes in
  Out_channel.printf "%s\n" pkh_hex ;
  let pks = List.filter_map cfg.Cfg.pks ~f:(fun pk -> Ec_public.of_hex (`Hex pk)) in
  let script = Script.create_multisig
      ~data:pkh_bytes ~threshold:cfg.threshold pks in
  match Payment_address.of_script ~version script with
  | None -> Out_channel.printf "Error while generating payment address"
  | Some addr -> Out_channel.printf "%s\n" (Payment_address.to_b58check addr)

let generate_one cfg version passphrase =
  let entropy = Random.Bytes.generate 20 in
  let words = Mnemonic.of_entropy entropy in
  printf "%s\n" (String.concat ~sep:" " words);
  let seed_bytes = String.subo ~len:32
      (Option.value_exn (Mnemonic.to_seed words ~passphrase)) in
  generate_seed cfg version seed_bytes

let generate cfg testnet n =
  let version =
    Payment_address.(if testnet then Testnet_P2SH else P2SH) in
  match getpass_confirm () with
  | None ->
      prerr_endline "Passphrase do not match. Aborting." ;
      Caml.exit 1
  | Some passphrase ->
      Random.stir () ;
      for i = 0 to n - 1 do
        generate_one cfg version passphrase
      done

let check cfg testnet words =
  let version =
    Payment_address.(if testnet then Testnet_P2SH else P2SH) in
  let passphrase = getpass () in
  match Mnemonic.to_seed ~passphrase words with
  | None ->
      prerr_endline "Provided mnemonic is invalid" ;
      Caml.exit 1
  | Some seed_bytes ->
      Out_channel.printf "%s\n" (String.concat ~sep:" " words);
      generate_seed cfg version (String.subo ~len:32 seed_bytes)

let generate =
  let doc = "Generate a Tezos wallet." in
  let n = Arg.(value & (pos 0 int 1) & info [] ~docv:"N") in
  Term.(const generate $ cfg $ testnet $ n),
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
