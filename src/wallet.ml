open Core
open Sodium
open Libbitcoin

open Util

let tezos_pkh_size = 20

let generate cfg_file testnet seed () =
  let version =
    Payment_address.(if testnet then Testnet_P2SH else P2SH) in
  let cfg =
    Option.value_map cfg_file ~default:Cfg.default ~f:begin fun fn ->
      Sexplib.Sexp.load_sexp_conv_exn fn Cfg.t_of_sexp
    end in
  Random.stir () ;
  let sk, pk = Sign.random_keypair () in
  let seed = Sign.secret_key_to_seed sk in
  let pk_bytes = Sign.Bytes.of_public_key pk in
  let `Hex seed_hex = Hex.of_string @@ Sign.Bytes.of_seed seed in
  let h = Generichash.init ~size:tezos_pkh_size () in
  Generichash.Bytes.update h pk_bytes ;
  let pkh = Generichash.final h in
  let pkh_bytes = Generichash.Bytes.of_hash pkh in
  let `Hex pkh_hex = Hex.of_string pkh_bytes in
  Out_channel.printf "%s\n" seed_hex ;
  Out_channel.printf "%s\n" pkh_hex ;
  let pks = List.filter_map cfg.pks ~f:Ec_public.of_base16 in
  let script = Script.create_multisig
      ~data:pkh_hex ~threshold:cfg.threshold pks in
  match Payment_address.of_script ~version script with
  | None -> Out_channel.printf "Error while generating payment address"
  | Some addr -> Out_channel.printf "%s\n" (Payment_address.to_b58check addr)

let check testnet (sk_encrypted, pkh, addr) () = ()

let generate =
  let spec =
    let open Command.Spec in
    empty
    +> flag "-cfg" (optional file) ~doc:"filename Configuration file"
    +> flag "-testnet" no_arg ~doc:" Use bitcoin testnet"
    +> flag "-seed" (optional string) ~doc:"string Use pre-generated private key"
  in
  Command.basic ~summary:"Generate a Tezos wallet" spec generate

let check =
  let spec =
    let open Command.Spec in
    empty
    +> flag "-testnet" no_arg ~doc:" Use bitcoin testnet"
    +> anon (t3 ("sk-encrypted" %: string) ("pkh" %: string) ("addr" %: string))
  in
  Command.basic ~summary:"Check a Tezos wallet" spec check

let command =
  Command.group ~summary:"Create a Tezos wallet" [
    "generate", generate;
    "check", check;
  ]

let () = Command.run command
