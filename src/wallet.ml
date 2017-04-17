open Core
open Sodium
open Libbitcoin

open Util

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
  Out_channel.printf "%s\n" (String.concat ~sep:" " words);
  let seed_bytes = String.subo ~len:32 @@
    Option.value_exn (Mnemonic.to_seed words ~passphrase) in
  generate_seed cfg version seed_bytes

let generate cfg_file testnet n () =
  let version =
    Payment_address.(if testnet then Testnet_P2SH else P2SH) in
  let cfg =
    Option.value_map cfg_file ~default:Cfg.default ~f:begin fun fn ->
      Sexplib.Sexp.load_sexp_conv_exn fn Cfg.t_of_sexp
    end in
  match getpass_confirm () with
  | None ->
      prerr_endline "Passphrase do not match. Aborting." ;
      exit 1
  | Some passphrase ->
      Random.stir () ;
      for i = 0 to n - 1 do
        generate_one cfg version passphrase
      done

let check cfg_file testnet words () =
  let version =
    Payment_address.(if testnet then Testnet_P2SH else P2SH) in
  let cfg =
    Option.value_map cfg_file ~default:Cfg.default ~f:begin fun fn ->
      Sexplib.Sexp.load_sexp_conv_exn fn Cfg.t_of_sexp
    end in
  let passphrase = getpass () in
  match Mnemonic.to_seed ~passphrase words with
  | None ->
      prerr_endline "Provided mnemonic is invalid" ;
      exit 1
  | Some seed_bytes ->
      Out_channel.printf "%s\n" (String.concat ~sep:" " words);
      generate_seed cfg version (String.subo ~len:32 seed_bytes)

let generate =
  let spec =
    let open Command.Spec in
    empty
    +> flag "-cfg" (optional file) ~doc:"filename Configuration file"
    +> flag "-testnet" no_arg ~doc:" Use bitcoin testnet"
    +> anon (maybe_with_default 1 ("number" %: int))
  in
  Command.basic ~summary:"Generate a Tezos wallet" spec generate

let check =
  let spec =
    let open Command.Spec in
    empty
    +> flag "-cfg" (optional file) ~doc:"filename Configuration file"
    +> flag "-testnet" no_arg ~doc:" Use bitcoin testnet"
    +> anon (sequence ("word" %: string))
  in
  Command.basic ~summary:"Check a Tezos wallet" spec check

let command =
  Command.group ~summary:"Create a Tezos wallet" [
    "generate", generate;
    "check", check;
  ]

let () = Command.run command
