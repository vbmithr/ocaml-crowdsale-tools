open Base
open Stdio
open Rresult
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

let amount_of_utxos =
  List.fold_left ~init:0 ~f:(fun acc utxo -> acc + utxo.Utxo.amount)

let output_of_dest_addr addr ~value =
  let script = Payment_address.to_script addr in
  Transaction.Output.create ~script ~value

let spend_n cfg testnet privkey pkhs amount =
  let dest_addrs = List.map pkhs
      ~f:(Fn.compose snd (script_and_payment_addr_of_pkh ~cfg ~testnet)) in
  let privkey = Ec_private.of_wif_exn privkey in
  let pubkey = Ec_public.of_private privkey in
  let secret = Ec_private.secret privkey in
  let version =
    if testnet then Payment_address.Testnet_P2PKH else P2PKH in
  let source = Payment_address.of_point ~version pubkey in
  let source_b58 = Payment_address.to_b58check source in
  utxos ~testnet [source_b58] >|=
  R.map begin fun utxos ->
    let utxos = List.filter utxos ~f:begin fun utxo ->
        match utxo.Utxo.confirmed with
        | Utxo.Unconfirmed _ -> false
        | _ -> true
      end in
    List.iter utxos ~f:begin fun utxo ->
      Lwt_log.ign_debug (Utxo.to_string utxo)
    end ;

    (* Build transaction *)

    let nb_dests = List.length dest_addrs in
    let inputs_and_scripts = List.map utxos ~f:input_and_script_of_utxo in
    let inputs = List.map inputs_and_scripts ~f:fst in
    let change_out =
      output_of_dest_addr source ~value:0L in
    let outputs =
      change_out :: List.map dest_addrs ~f:(output_of_dest_addr ~value:0L) in
    let tx = Transaction.create inputs outputs in

    (* Compute fees *)

    let size = Transaction.serialized_size tx in
    let fees_per_byte = if testnet then cfg.fees_testnet else cfg.fees in
    let fees = size * fees_per_byte in
    let amount_found = amount_of_utxos utxos in
    Lwt_log.ign_debug_f "Found %d utxos with total amount %d (%f)"
      (List.length utxos) amount_found (amount_found // 100_000_000) ;
    let total_amount = Option.value ~default:amount_found amount in
    let total_amount = Int.min amount_found total_amount in
    let change = amount_found - total_amount in
    let spendable_amount = total_amount - fees in
    let amount_per_addr = spendable_amount / nb_dests in

    Lwt_log.ign_debug_f "change = %d, spendable = %d, amount_per_addr = %d"
      change spendable_amount amount_per_addr ;

    List.iteri outputs ~f:begin fun i o ->
      let v = if i = 0 then change else amount_per_addr in
      Transaction.Output.set_value o (Int64.of_int v)
    end ;
    Transaction.set_outputs tx outputs ;

    (* Sign transaction *)

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
    printf "%s\n" tx_hex
  end

let spend_n loglevel cfg testnet privkey pkhs amount =
  set_loglevel loglevel ;
  let pkhs = match pkhs with
    | [] -> List.map Stdio.In_channel.(input_lines stdin)
              ~f:(fun s -> Hex.to_string (`Hex s))
    | _ -> pkhs in
  Lwt_main.run begin
    spend_n cfg testnet privkey pkhs amount >|= function
    | Ok () -> ()
    | Error err -> Lwt_log.ign_error (Http.string_of_error err)
  end

let spend_n =
  let amount =
    let doc = "Total amount to spend." in
    Arg.(value & opt (some int) None & info ["a" ; "amount"] ~doc) in
  let privkey =
    Arg.(required & (pos 0 (some string) None) & info [] ~docv:"PRIVKEY") in
  let pkhs =
    Arg.(value & (pos_right 1 Conv.hex []) & info [] ~docv:"DEST") in
  let doc = "Spend bitcoins equally between n addresses." in
  Term.(const spend_n $ loglevel $ cfg $ testnet $ privkey $ pkhs $ amount),
  Term.info ~doc "spend-n"

let amount_scriptRedeem_and_inputs_of_pkh ~cfg ~testnet pkh =
  let scriptRedeem, payment_addr = script_and_payment_addr_of_pkh ~cfg ~testnet pkh in
  let addr_b58 = Payment_address.to_b58check payment_addr in
  utxos ~testnet [addr_b58] >|=
  R.map begin fun utxos ->
    let amount = amount_of_utxos utxos in
    amount, scriptRedeem, List.map utxos ~f:(Fn.compose fst input_and_script_of_utxo)
  end

let spend_multisig cfg testnet pkhs dest =
  let dest = Payment_address.of_b58check_exn dest in
  let sk1, sk2 = match cfg.Cfg.sks with
    | sk1 :: sk2 :: _ -> sk1, sk2
    | _ -> failwith "Not enough sks" in
  let secret1, secret2 = Ec_private.(secret sk1, secret sk2) in
  Lwt_list.filter_map_s begin fun pkh ->
    amount_scriptRedeem_and_inputs_of_pkh ~cfg ~testnet pkh >|= R.to_option
  end pkhs >|= fun amounts_scripts_and_inputs ->
  let amounts, scriptRedeems, inputs = List.unzip3 amounts_scripts_and_inputs in
  let total_amount =
    List.fold_left amounts_scripts_and_inputs ~init:0 ~f:(fun acc (a,_,_) -> acc + a) in
  Lwt_log.ign_debug_f "Found %d utxos with total amount %d (%f)"
    (List.length amounts_scripts_and_inputs) total_amount (total_amount // 100_000_000) ;
  let output = output_of_dest_addr dest ~value:0L in
  let tx = Transaction.create (List.concat inputs) [output] in
  let fees_per_byte = if testnet then cfg.Cfg.fees_testnet else cfg.fees in
  let tx_size = Transaction.serialized_size tx in
  let spendable_amount = total_amount - tx_size * fees_per_byte in
  let output = output_of_dest_addr dest ~value:(Int64.of_int spendable_amount) in
  Transaction.set_outputs tx [output] ;
  let scriptSigs =
    List.mapi amounts_scripts_and_inputs ~f:begin fun index (amount, scriptRedeem, input) ->
      let open Transaction.Sign in
      let prev_out_script = Script.of_script scriptRedeem in
      let ed1 =
        Transaction.Sign.endorse_exn ~tx ~index ~prev_out_script ~secret:secret1 () in
      let ed2 =
        Transaction.Sign.endorse_exn ~tx ~index ~prev_out_script ~secret:secret2 () in
      Script.P2SH_multisig.scriptSig ~endorsements:[ ed1 ; ed2 ] ~scriptRedeem
    end in
  List.iter2_exn inputs scriptSigs ~f:begin fun inputs scriptSig ->
    List.iter inputs ~f:(fun input -> Transaction.Input.set_script input scriptSig)
  end ;
  Transaction.set_inputs tx (List.concat inputs) ;
  tx

let spend_multisig loglevel cfg testnet pkhs dest =
  set_loglevel loglevel ;
  let pkhs = match pkhs with
    | [] -> List.map Stdio.In_channel.(input_lines stdin)
              ~f:(fun s -> Hex.to_string (`Hex s))
    | _ -> pkhs in
  Lwt_main.run begin
    spend_multisig cfg testnet pkhs dest >|= fun tx ->
    let `Hex tx_hex = Transaction.to_hex tx in
    printf "%s\n" (Transaction.show tx) ;
    printf "%s\n" tx_hex
  end

let spend_multisig =
  let doc = "Spend bitcoins from a multisig address." in
  let dest =
    Arg.(required & (pos 0 (some Conv.payment_addr) None) & info [] ~docv:"DEST") in
  let pkhs =
    Arg.(value & (pos_right 1 Conv.hex []) & info [] ~docv:"PKHS") in
  Term.(const spend_multisig $ loglevel $ cfg $ testnet $ pkhs $ dest),
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
