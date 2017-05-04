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

module User = struct
  type t = {
    tezos_addr : Base58.Tezos.t ;
    scriptRedeem : Script.t ;
    payment_address : Base58.Bitcoin.t ;
  }

  let of_tezos_addr ~cfg ~testnet tezos_addr =
    let scriptRedeem =
      Script.P2SH_multisig.scriptRedeem
        ~append_script:Script.Script.Opcode.[Data tezos_addr.Base58.Tezos.payload ; Drop]
        ~threshold:cfg.Cfg.threshold cfg.pks in
    let scriptRedeem = Script.of_script scriptRedeem in
    let version =
      Base58.Bitcoin.(if testnet then Testnet_P2SH else P2SH) in
    let payment_address =
      Payment_address.to_b58check
        (Payment_address.of_script_exn ~version:version scriptRedeem) in
    { tezos_addr ; scriptRedeem ; payment_address }
end

let lookup_utxos loglevel cfg testnet tezos_addrs =
  set_loglevel loglevel ;
  let tezos_addrs =
    if List.is_empty tezos_addrs then
      List.map In_channel.(input_lines stdin) ~f:Base58.Tezos.of_string_exn else
      tezos_addrs in
  let run () =
    Lwt_log.debug_f "Found %d tezos addresses" (List.length tezos_addrs) >>= fun () ->
    let payment_addrs = List.map tezos_addrs ~f:begin fun tezos_addr ->
        let { User.payment_address } =
          User.of_tezos_addr ~cfg ~testnet tezos_addr in
        payment_address
      end in
    List.iter2_exn tezos_addrs payment_addrs ~f:begin fun ta pa ->
      Lwt_log.ign_debug_f "%s -> %s" (Base58.Tezos.show ta) (Base58.Bitcoin.show pa)
    end ;
    utxos ~testnet payment_addrs >>= function
    | Error err ->
      Lwt_log.error (Http.string_of_error err)
    | Ok utxos ->
      Lwt_log.debug_f "Blockexplorer: found %d utxo(s)" (List.length utxos) >|= fun () ->
      ignore begin List.iter2 payment_addrs utxos ~f:begin fun a u ->
          printf "%s\n" (Utxo.to_string u)
        end
      end in
  Lwt_main.run (run ())

let lookup_utxos =
  let doc = "Get Bitcoin UTXOs from a Tezos address." in
  let tezos_addrs =
    Arg.(value & (pos_all Conv.tezos_addr []) & info [] ~docv:"TEZOS_ADDR") in
  Term.(const lookup_utxos $ loglevel $ cfg $ testnet $ tezos_addrs),
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

let input_and_script_of_utxo ?(script = Script.invalid ()) utxo =
  match utxo.Utxo.confirmed with
  | Unconfirmed _ -> invalid_arg "input_of_utxo: unconfirmed"
  | Confirmed { vout ; scriptPubKey } ->
    let prev_out_hash = Hash.Hash32.of_hex_exn (`Hex utxo.Utxo.txid) in
    Transaction.Input.create ~prev_out_hash ~prev_out_index:vout ~script (),
    Option.value_exn (Script.of_hex (`Hex scriptPubKey))

let amount_of_utxos =
  List.fold_left ~init:0 ~f:(fun acc utxo -> acc + utxo.Utxo.amount)

let output_of_dest_addr addr ~value =
  let script = Payment_address.(of_b58check_exn addr |> to_script) in
  Transaction.Output.create ~script ~value

let spend_n cfg testnet privkey tezos_addrs amount =
  let dest_addrs = List.map tezos_addrs ~f:begin fun addr ->
      let { User.payment_address } = User.of_tezos_addr ~cfg ~testnet addr in
      payment_address
    end in
  let privkey = Ec_private.of_wif_exn privkey in
  let pubkey = Ec_public.of_private privkey in
  let secret = Ec_private.secret privkey in
  let version =
    if testnet then Base58.Bitcoin.Testnet_P2PKH else P2PKH in
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
      output_of_dest_addr source_b58 ~value:0L in
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

let spend_n loglevel cfg testnet privkey tezos_addrs amount =
  let amount =
    Option.map amount ~f:(fun a -> Int.of_float (Float.(a * 1e8))) in
  set_loglevel loglevel ;
  let tezos_addrs = match tezos_addrs with
    | [] -> List.map Stdio.In_channel.(input_lines stdin)
              ~f:Base58.Tezos.of_string_exn
    | _ -> tezos_addrs in
  Lwt_main.run begin
    spend_n cfg testnet privkey tezos_addrs amount >|= function
    | Ok () -> ()
    | Error err -> Lwt_log.ign_error (Http.string_of_error err)
  end

let spend_n =
  let amount =
    let doc = "Total amount to spend." in
    Arg.(value & opt (some float) None & info ["a" ; "amount"] ~doc) in
  let privkey =
    Arg.(required & (pos 0 (some string) None) & info [] ~docv:"PRIVKEY") in
  let tezos_addrs =
    Arg.(value & (pos_right 1 Conv.tezos_addr []) & info [] ~docv:"DEST") in
  let doc = "Spend bitcoins equally between n addresses." in
  Term.(const spend_n $ loglevel $ cfg $ testnet $ privkey $ tezos_addrs $ amount),
  Term.info ~doc "spend-n"

type tezos_addr_inputs = {
  tezos_addr : Base58.Tezos.t ;
  amount : int ;
  scriptRedeem : Script.t ;
  inputs : Transaction.Input.t list ;
}

let tezos_addr_inputs_encoding =
  let open Json_encoding in
  conv
    (fun { tezos_addr ; amount ; scriptRedeem ; inputs } ->
       let tezos_addr = Base58.Tezos.to_string tezos_addr in
       let `Hex scriptRedeem = Script.to_hex scriptRedeem in
       let inputs = List.map inputs
           ~f:(fun i -> let `Hex i_hex = Transaction.Input.to_hex i in i_hex) in
       (tezos_addr, amount, scriptRedeem, inputs))
    (fun (tezos_addr, amount, scriptRedeem, inputs) ->
       let tezos_addr = Base58.Tezos.of_string_exn tezos_addr in
       let scriptRedeem = Script.of_hex_exn (`Hex scriptRedeem) in
       let inputs = List.map inputs
           ~f:(fun i_hex -> Transaction.Input.of_hex_exn (`Hex i_hex)) in
       { tezos_addr ; scriptRedeem ; amount ; inputs })
    (obj4
       (req "tezos_addr" string)
       (req "amount" int)
       (req "scriptRedeem" string)
       (req "inputs" (list string)))

type tx = {
  tx : Transaction.t ;
  inputs: tezos_addr_inputs list ;
}

let tx_encoding =
  let open Json_encoding in
  conv
    (fun { tx ; inputs } ->
       let `Hex tx_hex = Transaction.to_hex tx in (tx_hex, inputs))
    (fun (tx, inputs) -> { tx = Transaction.of_hex_exn (`Hex tx) ; inputs })
    (obj2
       (req "tx" string)
       (req "inputs" (list tezos_addr_inputs_encoding)))

let inputs_of_tezos_addr ~cfg ~testnet tezos_addr =
  let { User.scriptRedeem ; payment_address } =
    User.of_tezos_addr ~cfg ~testnet tezos_addr in
  utxos ~testnet [payment_address] >|=
  R.map begin fun (utxos : Utxo.t list) ->
    List.fold_left utxos ~init:(0, []) ~f:begin fun (sum, inputs) utxo ->
      (sum + utxo.amount,
       (fst (input_and_script_of_utxo ~script:scriptRedeem utxo)) :: inputs)
    end
  end

let prepare_multisig_tx cfg testnet tezos_addrs dest =
  Lwt_list.filter_map_s begin fun tezos_addr ->
    inputs_of_tezos_addr ~cfg ~testnet tezos_addr >|= R.to_option
  end tezos_addrs >|= fun tezos_inputs ->
  let total_amount =
    List.fold_left tezos_inputs ~init:0 ~f:(fun acc (amount, _) -> acc + amount) in
  Lwt_log.ign_debug_f "Found %d utxos with total amount %d (%f)"
    (List.length tezos_inputs) total_amount (total_amount // 100_000_000) ;
  let output = output_of_dest_addr dest ~value:0L in
  let inputs = List.map tezos_inputs ~f:snd in
  let tx = Transaction.create (List.concat inputs) [output] in
  let fees_per_byte = if testnet then cfg.Cfg.fees_testnet else cfg.fees in
  let tx_size = Transaction.serialized_size tx in
  let spendable_amount = total_amount - tx_size * fees_per_byte in
  let output = output_of_dest_addr dest ~value:(Int64.of_int spendable_amount) in
  Transaction.set_outputs tx [output] ;
  tx

let prepare_multisig loglevel cfg testnet tezos_addrs dest =
  set_loglevel loglevel ;
  let tezos_addrs = match tezos_addrs with
    | [] -> List.map Stdio.In_channel.(input_lines stdin)
              ~f:Base58.Tezos.of_string_exn
    | _ -> tezos_addrs in
  Lwt_main.run begin
    prepare_multisig_tx cfg testnet tezos_addrs dest >|= fun tx ->
    let `Hex tx_hex = Transaction.to_hex tx in
    printf "%s\n" (Transaction.show tx) ;
    printf "%s\n" tx_hex
  end

let prepare_multisig =
  let doc = "Prepare a transaction for Ledger." in
  let dest =
    Arg.(required & (pos 0 (some Conv.payment_addr) None) & info [] ~docv:"DEST") in
  let tezos_addrs =
    Arg.(value & (pos_right 1 Conv.tezos_addr []) & info [] ~docv:"TEZOS_ADDRS") in
  Term.(const prepare_multisig $ loglevel $ cfg $ testnet $ tezos_addrs $ dest),
  Term.info ~doc "prepare-multisig"

let spend_multisig cfg testnet tezos_addrs dest =
  prepare_multisig_tx cfg testnet tezos_addrs dest >|= fun tx ->
  let sk1, sk2 = match cfg.Cfg.sks with
    | sk1 :: sk2 :: _ -> sk1, sk2
    | _ -> failwith "Not enough sks" in
  let secret1, secret2 = Ec_private.(secret sk1, secret sk2) in
  let inputs =
    List.mapi (Transaction.get_inputs tx) ~f:begin fun index input ->
      let open Transaction.Sign in
      let prev_out_script = Transaction.Input.get_script input in
      let ed1 =
        Transaction.Sign.endorse_exn ~tx ~index ~prev_out_script ~secret:secret1 () in
      let ed2 =
        Transaction.Sign.endorse_exn ~tx ~index ~prev_out_script ~secret:secret2 () in
      let scriptSig =
        Script.P2SH_multisig.scriptSig
          ~endorsements:[ ed1 ; ed2 ]
          ~scriptRedeem:prev_out_script in
      Transaction.Input.set_script input scriptSig ;
      input
    end in
  Transaction.set_inputs tx inputs ;
  tx

let spend_multisig loglevel cfg testnet tezos_addrs dest =
  set_loglevel loglevel ;
  let tezos_addrs = match tezos_addrs with
    | [] -> List.map Stdio.In_channel.(input_lines stdin)
              ~f:Base58.Tezos.of_string_exn
    | _ -> tezos_addrs in
  Lwt_main.run begin
    spend_multisig cfg testnet tezos_addrs dest >|= fun tx ->
    let `Hex tx_hex = Transaction.to_hex tx in
    printf "%s\n" (Transaction.show tx) ;
    printf "%s\n" tx_hex
  end

let spend_multisig =
  let doc = "Spend bitcoins from a multisig address." in
  let dest =
    Arg.(required & (pos 0 (some Conv.payment_addr) None) & info [] ~docv:"DEST") in
  let tezos_addrs =
    Arg.(value & (pos_right 1 Conv.tezos_addr []) & info [] ~docv:"TEZOS_ADDRS") in
  Term.(const spend_multisig $ loglevel $ cfg $ testnet $ tezos_addrs $ dest),
  Term.info ~doc "spend-multisig"

let endorse_multisig cfg testnet tezos_addrs dest =
  prepare_multisig_tx cfg testnet tezos_addrs dest >|= fun tx ->
  let sk = match cfg.Cfg.sks with
    | sk :: _ -> sk
    | _ -> failwith "Not enough sks" in
  let secret = Ec_private.secret sk in
  List.mapi (Transaction.get_inputs tx) ~f:begin fun index input ->
    let open Transaction.Sign in
    let prev_out_script = Transaction.Input.get_script input in
    Transaction.Sign.endorse_exn ~tx ~index ~prev_out_script ~secret ()
  end

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
  prepare_multisig ;
  spend_multisig ;
]

let () = match Term.eval_choice default_cmd cmds with
  | `Error _ -> Caml.exit 1
  | #Term.result -> Caml.exit 0
