open Lwt.Infix
open Cmdliner
open Libbitcoin
open Blockexplorer
open Blockexplorer_lwt
open Util
open Util.Cmdliner

let tx_decode loglevel tx =
  set_loglevel loglevel ;
  let tx = match tx with
    | None -> Hex.to_string (`Hex Stdio.In_channel.(input_line_exn stdin))
    | Some tx -> tx in
  match Transaction.of_hex (Hex.of_string tx) with
  | None -> prerr_endline "Unable to decode"
  | Some tx -> Caml.Format.printf "%a@." Transaction.pp tx

let tx_decode =
  let doc = "Decode a Base16 transaction." in
  let tx =
    Arg.(value & (pos 0 (some Conv.hex) None) & info [] ~docv:"TX") in
  Term.(const tx_decode $ loglevel $ tx),
  Term.info ~doc "tx-decode"

let script_decode loglevel rawscript =
  set_loglevel loglevel ;
  match Script.of_hex (`Hex rawscript) with
  | None -> prerr_endline "Unable to decode"
  | Some script -> Caml.Format.printf "%a@." Script.pp script

let script_decode =
  let doc = "Decode a script to plain text tokens." in
  let script =
    Arg.(required & (pos 0 (some string) None) & info [] ~docv:"SCRIPT") in
  Term.(const script_decode $ loglevel $ script),
  Term.info ~doc "script-decode"

let tx_broadcast loglevel testnet rawtx_bytes =
  set_loglevel loglevel ;
  let rawtx_bytes = match rawtx_bytes with
    | None -> Hex.to_string (`Hex Stdio.In_channel.(input_line_exn stdin))
    | Some tx -> tx in
  let run () =
    broadcast_tx ~testnet rawtx_bytes >>= function
    | Ok (`Hex txid) -> Lwt_log.info txid
    | Error err -> Lwt_log.error (Http.string_of_error err) in
  Lwt_main.run (run ())

let tx_broadcast =
  let doc = "Broadcast a transaction with blockexplorer.com API." in
  let tx =
    Arg.(value & (pos 0 (some Conv.hex) None) & info [] ~docv:"TX") in
  Term.(const tx_broadcast $ loglevel $ testnet $ tx),
  Term.info ~doc "tx-broadcast"

let tx_fetch loglevel testnet txid =
  set_loglevel loglevel ;
  let run () =
    rawtx ~testnet (Hex.of_string txid) >>= function
    | Error err -> Lwt_log.error (Http.string_of_error err)
    | Ok t -> begin
        match Transaction.of_bytes t with
        | None -> Lwt_log.error "Unable to decode raw transaction"
        | Some tx ->
          let tx_decoded = Caml.Format.asprintf "%a" Transaction.pp tx in
          Lwt_log.info tx_decoded
      end in
  Lwt_main.run (run ())

let tx_fetch =
  let doc = "Fetch a transaction from blockexplorer.com API." in
  let txid =
    Arg.(required & (pos 0 (some Conv.hex) None) & info [] ~docv:"TXID") in
  Term.(const tx_fetch $ loglevel $ testnet $ txid),
  Term.info ~doc "tx-fetch"

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

let ec_compress loglevel testnet pubkey =
  let pubkey = Ec_public.of_uncomp_point_exn pubkey in
  let version = Base58.Bitcoin.(if testnet then Testnet_P2PKH else P2PKH) in
  let addr = Payment_address.of_point ~version pubkey in
  Format.printf "%a@.%a@."
    Ec_public.pp pubkey Payment_address.pp addr

let ec_compress =
  let doc = "Convert an EC public key to a payment address." in
  let ec =
    Arg.(required & (pos 0 (some Conv.hex) None) & info [] ~docv:"UNCOMPRESSED_EC_PUBLIC_KEY") in
  Term.(const ec_compress $ loglevel $ testnet $ ec),
  Term.info ~doc "ec-compress"

let default_cmd =
  let doc = "Bitcoin tools." in
  Term.(ret (const (`Help (`Pager, None)))),
  Term.info ~doc "obx"

let cmds = [
  ec_to_wif ;
  ec_to_public ;
  ec_to_address ;
  ec_compress ;
  tx_decode ;
  tx_fetch ;
  tx_broadcast ;
  script_decode ;
]

let () = match Term.eval_choice default_cmd cmds with
  | `Error _ -> Caml.exit 1
  | #Term.result -> Caml.exit 0
