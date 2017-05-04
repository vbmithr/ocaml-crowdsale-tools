open Lwt.Infix
open Cmdliner
open Libbitcoin
open Blockexplorer
open Blockexplorer_lwt
open Util
open Util.Cmdliner

let decode_tx loglevel rawtx =
  set_loglevel loglevel ;
  match Transaction.of_hex (Hex.of_string rawtx) with
  | None -> prerr_endline "Unable to decode"
  | Some tx -> Caml.Format.printf "%a@." Transaction.pp tx

let decode_tx =
  let doc = "Decode and print a transaction in raw format." in
  let rawtx =
    Arg.(required & (pos 0 (some Conv.hex) None) & info [] ~docv:"RAWTX") in
  Term.(const decode_tx $ loglevel $ rawtx),
  Term.info ~doc "decode-tx"

let decode_script loglevel rawscript =
  set_loglevel loglevel ;
  match Script.of_hex (`Hex rawscript) with
  | None -> prerr_endline "Unable to decode"
  | Some script -> Caml.Format.printf "%a@." Script.pp script

let decode_script =
  let doc = "Decode and print a script in raw format." in
  let script =
    Arg.(required & (pos 0 (some string) None) & info [] ~docv:"SCRIPT") in
  Term.(const decode_script $ loglevel $ script),
  Term.info ~doc "decode-script"

let broadcast_tx loglevel testnet rawtx =
  set_loglevel loglevel ;
  let run () =
    broadcast_tx ~testnet (Hex.of_string rawtx) >>= function
    | Ok (`Hex txid) -> Lwt_log.info txid
    | Error err -> Lwt_log.error (Http.string_of_error err) in
  Lwt_main.run (run ())

let broadcast_tx =
  let doc = "Broadcast a transaction with blockexplorer.com API." in
  let rawtx =
    Arg.(required & (pos 0 (some Conv.hex) None) & info [] ~docv:"RAWTX") in
  Term.(const broadcast_tx $ loglevel $ testnet $ rawtx),
  Term.info ~doc "broadcast-tx"

let fetch_tx loglevel testnet txid =
  set_loglevel loglevel ;
  let run () =
    rawtx ~testnet (Hex.of_string txid) >>= function
    | Error err -> Lwt_log.error (Http.string_of_error err)
    | Ok t -> begin
        match Transaction.of_hex t with
        | None -> Lwt_log.error "Unable to decode raw transaction"
        | Some tx ->
          let tx_decoded = Caml.Format.asprintf "%a" Transaction.pp tx in
          Lwt_log.info tx_decoded
      end in
  Lwt_main.run (run ())

let fetch_tx =
  let doc = "Fetch a transaction from blockexplorer.com API." in
  let txid =
    Arg.(required & (pos 0 (some Conv.hex) None) & info [] ~docv:"TXID") in
  Term.(const fetch_tx $ loglevel $ testnet $ txid),
  Term.info ~doc "fetch-tx"

let ec_to_wif loglevel uncompressed testnet ec =
  let compress = not uncompressed in
  Printf.printf "%s\n"
    Ec_private.(of_secret ~compress ~testnet (Ec_secret.of_bytes ec) |> to_wif)

let ec_to_wif =
  let doc = "Convert an EC private key to a WIF private key." in
  let uncompressed =
    let doc = "Associate the result with the uncompressed public key format." in
    Arg.(value & flag & info ["u" ; "uncompressed"] ~doc) in
  let ec =
    Arg.(required & (pos 0 (some Conv.hex) None) & info [] ~docv:"EC_PRIVATE _KEY") in
  Term.(const ec_to_wif $ loglevel $ uncompressed $ testnet $ ec),
  Term.info ~doc "ec-to-wif"

let ec_to_public loglevel uncompressed testnet ec =
  let compress = not uncompressed in
  let `Hex ec_public_hex =
    Ec_private.(of_secret ~compress ~testnet (Ec_secret.of_bytes ec) |>
                Ec_public.of_private |> Ec_public.to_hex) in
    Printf.printf "%s\n" ec_public_hex

let ec_to_public =
  let doc = "Derive the EC public key of an EC private key." in
  let uncompressed =
    let doc = "Associate the result with the uncompressed public key format." in
    Arg.(value & flag & info ["u" ; "uncompressed"] ~doc) in
  let ec =
    Arg.(required & (pos 0 (some Conv.hex) None) & info [] ~docv:"EC_PRIVATE_KEY") in
  Term.(const ec_to_public $ loglevel $ uncompressed $ testnet $ ec),
  Term.info ~doc "ec-to-public"

let ec_to_address loglevel testnet ec =
  let ec = Ec_public.of_bytes_exn ec in
  let version = Base58.Bitcoin.(if testnet then Testnet_P2PKH else P2PKH) in
  let addr_b58 = (Payment_address.of_point ~version ec |> Payment_address.to_b58check) in
  Format.printf "%a@." Base58.Bitcoin.pp addr_b58

let ec_to_address =
  let doc = "Convert an EC public key to a payment address." in
  let ec =
    Arg.(required & (pos 0 (some Conv.hex) None) & info [] ~docv:"EC_PUBLIC_KEY") in
  Term.(const ec_to_address $ loglevel $ testnet $ ec),
  Term.info ~doc "ec-to-address"

let default_cmd =
  let doc = "Bitcoin tools." in
  Term.(ret (const (`Help (`Pager, None)))),
  Term.info ~doc "obx"

let cmds = [
  ec_to_wif ;
  ec_to_public ;
  ec_to_address ;
  decode_tx ;
  decode_script ;
  fetch_tx ;
  broadcast_tx ;
]

let () = match Term.eval_choice default_cmd cmds with
  | `Error _ -> Caml.exit 1
  | #Term.result -> Caml.exit 0
