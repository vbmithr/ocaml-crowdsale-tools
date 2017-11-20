open Lwt.Infix
open Cmdliner
open Blockexplorer
open Blockexplorer_lwt
open Util
open Util.Cmdliner

open Bitcoin.Protocol

let blk_decode loglevel blk =
  set_loglevel loglevel ;
  let blk = match blk with
    | None -> Hex.to_cstruct (`Hex Stdio.In_channel.(input_line_exn stdin))
    | Some blk -> blk
  in
  let block, _cs = Block.of_cstruct blk in
  Caml.Format.printf "%a@." Block.pp block

let blk_decode =
  let doc = "Decode a block in binary or Base16 format." in
  (* let binary =
   *   let doc = "Block is in binary format." in
   *   Arg.(value & flag & info ["b" ; "binary"] ~doc) in *)
  let blk =
    Arg.(value & (pos 0 (some Conv.hex_cstruct) None) & info [] ~docv:"BLOCK") in
  Term.(const blk_decode $ loglevel $ blk),
  Term.info ~doc "blk-decode"

let tx_decode loglevel tx =
  set_loglevel loglevel ;
  let tx = match tx with
    | None -> Hex.to_cstruct (`Hex Stdio.In_channel.(input_line_exn stdin))
    | Some tx -> tx in
  let tx, _cs = Transaction.of_cstruct tx in
  Caml.Format.printf "%a@." Transaction.pp tx

let tx_decode =
  let doc = "Decode a Base16 transaction." in
  let tx =
    Arg.(value & (pos 0 (some Conv.hex_cstruct) None) & info [] ~docv:"TX") in
  Term.(const tx_decode $ loglevel $ tx),
  Term.info ~doc "tx-decode"

(* let script_decode loglevel rawscript =
 *   set_loglevel loglevel ;
 *   match Script.of_hex (`Hex rawscript) with
 *   | None -> prerr_endline "Unable to decode"
 *   | Some script -> Caml.Format.printf "%a@." Script.pp script
 * 
 * let script_decode =
 *   let doc = "Decode a script to plain text tokens." in
 *   let script =
 *     Arg.(required & (pos 0 (some string) None) & info [] ~docv:"SCRIPT") in
 *   Term.(const script_decode $ loglevel $ script),
 *   Term.info ~doc "script-decode" *)

let tx_broadcast loglevel network rawtx_bytes =
  set_loglevel loglevel ;
  let rawtx_bytes = match rawtx_bytes with
    | None -> Hex.to_string (`Hex Stdio.In_channel.(input_line_exn stdin))
    | Some tx -> tx in
  let run () =
    broadcast_tx ~network rawtx_bytes >>= function
    | Ok (`Hex txid) -> Lwt_log.info txid
    | Error err -> Lwt_log.error (Http.string_of_error err) in
  Lwt_main.run (run ())

let tx_broadcast =
  let doc = "Broadcast a transaction with blockexplorer.com API." in
  let tx =
    Arg.(value & (pos 0 (some Conv.hex) None) & info [] ~docv:"TX") in
  Term.(const tx_broadcast $ loglevel $ network $ tx),
  Term.info ~doc "tx-broadcast"

let tx_fetch loglevel network txid =
  set_loglevel loglevel ;
  let run () =
    rawtx ~network (Hex.of_string txid) >>= function
    | Error err -> Lwt_log.error (Http.string_of_error err)
    | Ok t -> begin
        let tx, _cs = Transaction.of_cstruct (Cstruct.of_string t) in
        let tx_decoded = Caml.Format.asprintf "%a" Transaction.pp tx in
        Lwt_log.info tx_decoded
      end in
  Lwt_main.run (run ())

let tx_fetch =
  let doc = "Fetch a transaction from blockexplorer.com API." in
  let txid =
    Arg.(required & (pos 0 (some Conv.hex) None) & info [] ~docv:"TXID") in
  Term.(const tx_fetch $ loglevel $ network $ txid),
  Term.info ~doc "tx-fetch"

let ec_to_wif loglevel uncompressed testnet secret =
  let open Bitcoin.Wallet in
  let secret =
    Secp256k1.Secret.read_exn ctx secret.Cstruct.buffer in
  let compress = not uncompressed in
  let wif = WIF.create ~testnet ~compress secret in
  Format.printf "%a@." WIF.pp wif

let ec_to_wif =
  let doc = "Encode an secp256k1 private key to WIF format." in
  let uncompressed =
    let doc = "Associate the result with the uncompressed public key format." in
    Arg.(value & flag & info ["u" ; "uncompressed"] ~doc) in
  let ec =
    Arg.(required & (pos 0 (some Conv.hex_cstruct) None) & info [] ~docv:"BYTES") in
  Term.(const ec_to_wif $ loglevel $ uncompressed $ testnet $ ec),
  Term.info ~doc "ec-to-wif"

let ec_to_public loglevel uncompressed secret =
  let open Bitcoin.Wallet in
  let compress = not uncompressed in
  let secret =
    Secp256k1.Secret.read_exn ctx secret.Cstruct.buffer in
  let public =
    Secp256k1.Public.of_secret ctx secret in
  let public_bytes =
    Secp256k1.Public.to_bytes ~compress ctx public |> Cstruct.of_bigarray in
  let `Hex ec_public_hex = Hex.of_cstruct public_bytes in
  Printf.printf "%s\n" ec_public_hex

let ec_to_public =
  let doc = "Derive the secp256k1 public key of a secp256k1 private key." in
  let uncompressed =
    let doc = "Associate the result with the uncompressed public key format." in
    Arg.(value & flag & info ["u" ; "uncompressed"] ~doc) in
  let ec =
    Arg.(required & (pos 0 (some Conv.hex_cstruct) None) & info [] ~docv:"BYTES") in
  Term.(const ec_to_public $ loglevel $ uncompressed $ ec),
  Term.info ~doc "ec-to-public"

let ec_to_address loglevel testnet pk =
  let public = Secp256k1.Public.read_exn ctx pk.Cstruct.buffer in
  let addr = Bitcoin.Wallet.Address.of_pubkey ~testnet ctx public in
  Format.printf "%a@." Base58.Bitcoin.pp addr

let ec_to_address =
  let doc = "Convert an EC public key to a payment address." in
  let ec =
    Arg.(required & (pos 0 (some Conv.hex_cstruct) None) & info [] ~docv:"EC_PUBLIC_KEY") in
  Term.(const ec_to_address $ loglevel $ testnet $ ec),
  Term.info ~doc "ec-to-address"

let ec_compress loglevel testnet pk =
  let public = Secp256k1.Public.read_exn ctx pk.Cstruct.buffer in
  let `Hex pk_hex =
    Secp256k1.Public.to_bytes ctx public |> Cstruct.of_bigarray |> Hex.of_cstruct in
  let addr = Bitcoin.Wallet.Address.of_pubkey ~testnet ctx public in
  Format.printf "%s@.%a@." pk_hex Base58.Bitcoin.pp addr

let ec_compress =
  let doc = "Convert an EC public key to a payment address." in
  let ec =
    Arg.(required & (pos 0 (some Conv.hex_cstruct) None) & info [] ~docv:"UNCOMPRESSED_EC_PUBLIC_KEY") in
  Term.(const ec_compress $ loglevel $ testnet $ ec),
  Term.info ~doc "ec-compress"

let get_wallet_pubkey path =
  let open Ledgerwallet in
  let h = Hidapi.hid_open ~vendor_id ~product_id in
  let { Public_key.uncompressed } = get_wallet_public_key h path in
  let pk = Secp256k1.Public.read_exn ctx uncompressed.Cstruct.buffer in
  let addr = Bitcoin.Wallet.Address.of_pubkey ctx pk in
  let addr_testnet = Bitcoin.Wallet.Address.of_pubkey ~testnet:true ctx pk in
  Base58.Bitcoin.(Format.printf "%a@.%a@." pp addr pp addr_testnet)

let get_wallet_pubkey =
  let doc = "Get Ledger pubkey from a BIP44 path." in
  let path =
    Arg.(required & (pos 0 (some Conv.path) None) & info [] ~docv:"BIP44_PATH") in
  Term.(const get_wallet_pubkey $ path),
  Term.info ~doc "ledger-get-wallet-pubkey"

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
  (* script_decode ; *)
  blk_decode ;
  get_wallet_pubkey ;
]

let () = match Term.eval_choice default_cmd cmds with
  | `Error _ -> Caml.exit 1
  | #Term.result -> Caml.exit 0
