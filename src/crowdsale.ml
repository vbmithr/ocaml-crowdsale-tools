open Base
open Stdio
open Cmdliner
open Lwt.Infix

open Libbitcoin
open Blockexplorer
open Blockexplorer.Types
open Blockexplorer_lwt

open Util
open Util.Cmdliner

let script_and_payment_addr_of_pkh ~cfg ~testnet pkh =
  let scriptRedeem =
    Script.P2SH_multisig.scriptRedeem
      ~append_script:Script.Script.Opcode.[Data pkh ; Drop]
      ~threshold:cfg.Cfg.threshold cfg.pks in
  let version =
    Payment_address.(if testnet then Testnet_P2SH else P2SH) in
  scriptRedeem,
  Payment_address.of_script_exn ~version (Script.of_script scriptRedeem)

let lookup_utxos loglevel cfg testnet pkhs =
  set_loglevel loglevel ;
  let pkhs =
    if List.is_empty pkhs then In_channel.(input_lines stdin) else pkhs in
  let run () =
    Lwt_log.debug_f "Found %d pkhs" (List.length pkhs) >>= fun () ->
    let addrs = List.map pkhs ~f:begin fun pkh ->
        let _script, addr = script_and_payment_addr_of_pkh ~cfg ~testnet pkh in
        Payment_address.to_b58check addr
      end in
    List.iter2_exn pkhs addrs ~f:begin fun pkh (`Base58 addr) ->
      Lwt_log.ign_debug_f "%s -> %s" pkh addr
    end ;
    utxos ~testnet addrs >>= function
    | Error err ->
      Lwt_log.error (Http.string_of_error err)
    | Ok utxos ->
      Lwt_log.debug_f "Blockexplorer: found %d utxo(s)" (List.length utxos) >|= fun () ->
      ignore begin List.iter2 addrs utxos ~f:begin fun a u ->
          printf "%s" (Utxo.to_string u)
        end
      end in
  Lwt_main.run (run ())

let lookup_utxos =
  let doc = "Get Bitcoin UTXOs from a Tezos public key hash." in
  let pkhs =
    Arg.(non_empty & (pos_all Conv.hex []) & info [] ~docv:"pkhs") in
  Term.(const lookup_utxos $ loglevel $ cfg $ testnet $ pkhs),
  Term.info ~doc "lookup-utxos"

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

let input_and_script_of_utxo utxo = match utxo.Utxo.confirmed with
  | Unconfirmed _ -> invalid_arg "input_of_utxo: unconfirmed"
  | Confirmed { vout ; scriptPubKey } ->
    let prev_out_hash = Hash.Hash32.of_hex_exn (`Hex utxo.Utxo.txid) in
    let script = Script.invalid () in
    Transaction.Input.create ~prev_out_hash ~prev_out_index:vout ~script (),
    Option.value_exn (Script.of_hex (`Hex scriptPubKey))

let spend_n loglevel cfg testnet privkey source dest_addrs amount fees =
  set_loglevel loglevel ;
  let amount_of_utxos =
    List.fold_left ~init:0 ~f:(fun acc utxo -> acc + utxo.Utxo.amount) in
  let output_of_dest_addr addr ~value =
    let script = Payment_address.to_script addr in
    Transaction.Output.create ~script ~value in
  let dest_addrs = List.map dest_addrs ~f:Payment_address.of_b58check_exn in
  let privkey = Ec_private.of_wif_exn privkey in
  let pubkey = Ec_public.of_private privkey in
  let secret = Ec_private.secret privkey in
  let run () =
    utxos ~testnet [source] >>= function
    | Error err -> Lwt_log.error (Http.string_of_error err)
    | Ok utxos ->
      let utxos = List.filter utxos ~f:begin fun utxo ->
          match utxo.Utxo.confirmed with
          | Utxo.Unconfirmed _ -> false
          | _ -> true
        end in
      List.iter utxos ~f:begin fun utxo ->
        Lwt_log.ign_debug (Utxo.to_string utxo)
      end ;
      let nb_dests = List.length dest_addrs in
      let amount_found = amount_of_utxos utxos in
      let total_amount = Option.value ~default:amount_found amount in
      Lwt_log.ign_debug_f "Found %d utxos with total amount %d (%f)"
        (List.length utxos) amount_found (amount_found // 100_000_000) ;
      let fees = Option.value ~default:100_000 fees in
      let change = amount_found - total_amount - fees in
      let amount_per_addr = Int64.of_int ((total_amount - fees) / nb_dests) in
      let inputs_and_scripts = List.map utxos ~f:input_and_script_of_utxo in
      let inputs = List.map inputs_and_scripts ~f:fst in
      let change_out =
        output_of_dest_addr (Payment_address.of_b58check_exn source)
          ~value:(Int64.of_int change) in
      let outputs =
        change_out :: List.map dest_addrs ~f:(output_of_dest_addr ~value:amount_per_addr) in
      let tx = Transaction.create inputs outputs in
      let scriptSigs =
        List.mapi inputs_and_scripts ~f:begin fun index (_input, prev_out_script) ->
          let open Transaction.Sign in
          let endorsement =
            Transaction.Sign.endorse_exn ~tx ~index ~prev_out_script ~secret () in
          Script.P2PKH.scriptSig endorsement pubkey
        end in
      List.iter2_exn inputs scriptSigs ~f:begin fun i e ->
        Transaction.Input.set_script i e
      end ;
      Transaction.set_inputs tx inputs ;
      let `Hex tx_hex = Transaction.to_hex tx in
      printf "%s\n" (Transaction.show tx) ;
      printf "%s\n" tx_hex ;
      Lwt.return_unit in
  Lwt_main.run (run ())

let spend_n =
  let amount =
    let doc = "Total amount to spend." in
    Arg.(value & opt (some int) None & info ["a" ; "amount"] ~doc) in
  let fees =
    let doc = "Fees for the transaction." in
    Arg.(value & opt (some int) None & info ["f" ; "fees"] ~doc) in
  let privkey =
    Arg.(required & (pos 0 (some string) None) & info [] ~docv:"PRIVKEY") in
  let source =
    Arg.(required & (pos 1 (some Conv.payment_addr) None) & info [] ~docv:"SOURCE") in
  let dests =
    Arg.(non_empty & (pos_right 1 Conv.payment_addr []) & info [] ~docv:"DEST") in
  let doc = "Spend bitcoins equally between n addresses." in
  Term.(const spend_n $ loglevel $ cfg $ testnet $ privkey $ source $ dests $ amount $ fees),
  Term.info ~doc "spend-n"

let spend_multisig loglevel cfg testnet (sk1, sk2) pkh dest =
  let scriptRedeem, source = script_and_payment_addr_of_pkh ~cfg ~testnet pkh in
  set_loglevel loglevel ;
  let amount_of_utxos =
    List.fold_left ~init:0 ~f:(fun acc utxo -> acc + utxo.Utxo.amount) in
  let output_of_dest_addr addr ~value =
    let script = Payment_address.to_script addr in
    Transaction.Output.create ~script ~value in
  let dest = Payment_address.of_b58check_exn dest in
  let sk1, sk2 = Ec_private.(of_wif_exn sk1, of_wif_exn sk2) in
  let secret1, secret2 = Ec_private.(secret sk1, secret sk2) in
  let run () =
    utxos ~testnet [Payment_address.to_b58check source] >>= function
    | Error err -> Lwt_log.error (Http.string_of_error err)
    | Ok utxos ->
      let utxos = List.filter utxos ~f:begin fun utxo ->
          match utxo.Utxo.confirmed with
          | Utxo.Unconfirmed _ -> false
          | _ -> true
        end in
      List.iter utxos ~f:begin fun utxo ->
        Lwt_log.ign_debug (Utxo.to_string utxo)
      end ;
      let total_amount = amount_of_utxos utxos in
      Lwt_log.ign_debug_f "Found %d utxos with total amount %d (%f)"
        (List.length utxos) total_amount (total_amount // 100_000_000) ;
      let total_spendable = Int64.of_int (total_amount - 100_000) in
      let inputs_and_scripts = List.map utxos ~f:input_and_script_of_utxo in
      let inputs = List.map inputs_and_scripts ~f:fst in
      let output = output_of_dest_addr dest ~value:total_spendable in
      let tx = Transaction.create inputs [output] in
      let scriptSigs =
        List.mapi inputs_and_scripts ~f:begin fun index (input, prev_out_script) ->
          let open Transaction.Sign in
          let ed1 =
            Transaction.Sign.endorse_exn ~tx ~index ~prev_out_script ~secret:secret1 () in
          let ed2 =
            Transaction.Sign.endorse_exn ~tx ~index ~prev_out_script ~secret:secret2 () in
          Script.P2SH_multisig.scriptSig ~endorsements:[ ed1 ; ed2 ] ~scriptRedeem
        end in
      List.iter2_exn inputs scriptSigs ~f:begin fun i e ->
        Transaction.Input.set_script i e
      end ;
      Transaction.set_inputs tx inputs ;
      let `Hex tx_hex = Transaction.to_hex tx in
      printf "%s\n" (Transaction.show tx) ;
      printf "%s\n" tx_hex ;
      Lwt.return_unit in
  Lwt_main.run (run ())

let spend_multisig =
  let doc = "Spend bitcoins from a multisig address." in
  let privkey =
    Arg.(required & (pos 0 (some (t2 string string)) None) & info [] ~docv:"SECRETKEYS") in
  let source =
    Arg.(required & (pos 1 (some Conv.hex) None) & info [] ~docv:"PKH") in
  let dest =
    Arg.(required & (pos 2 (some Conv.payment_addr) None) & info [] ~docv:"DEST") in
  Term.(const spend_multisig $ loglevel $ cfg $ testnet $ privkey $ source $ dest),
  Term.info ~doc "spend-multisig"

let default_cmd =
  let doc = "Crowdsale tools." in
  Term.(ret (const (`Help (`Pager, None)))),
  Term.info ~doc "crowdsale"

let cmds = [
  lookup_utxos ;
  broadcast_tx ;
  fetch_tx ;
  decode_tx ;
  decode_script ;
  spend_n ;
  spend_multisig ;
]

let () = match Term.eval_choice default_cmd cmds with
  | `Error _ -> Caml.exit 1
  | #Term.result -> Caml.exit 0
