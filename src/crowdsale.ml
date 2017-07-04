open Base
open Stdio
open Rresult
open Cmdliner
open Lwt.Infix

open Libbitcoin
open Blockexplorer
open Blockexplorer.Types
module BL = Blockexplorer_lwt

open Util
open Util.Cmdliner

let input_and_script_of_utxo
    ?(min_confirmations = 1)
    ?(script = Script.invalid ())
    utxo =
  match utxo.Utxo.confirmed with
  | Unconfirmed _ -> None
  | Confirmed { confirmations } when confirmations < min_confirmations -> None
  | Confirmed { vout ; scriptPubKey } ->
    let prev_out_hash = Hash.Hash32.of_hex_exn utxo.Utxo.txid in
    Some
      (Transaction.Input.create ~prev_out_hash ~prev_out_index:vout ~script (),
       Script.of_hex_exn (`Hex scriptPubKey))

let amount_of_utxos =
  List.fold_left ~init:0 ~f:(fun acc utxo -> acc + utxo.Utxo.amount)

let output_of_dest_addr addr ~value =
  let script = Payment_address.(of_b58check_exn addr |> to_script) in
  Transaction.Output.create ~script ~value

let spend_n cfg privkey tezos_addrs amount =
  let dest_addrs = List.map tezos_addrs ~f:begin fun addr ->
      let { User.payment_address } = User.of_tezos_addr ~cfg addr in
      payment_address
    end in
  let privkey = Ec_private.of_wif_exn privkey in
  let pubkey = Ec_public.of_private privkey in
  let secret = Ec_private.secret privkey in
  let source = Payment_address.of_point pubkey in
  let source_b58 = Payment_address.to_b58check source in
  Blockexplorer_lwt.utxos [source_b58] >|=
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
    let inputs_and_scripts =
      List.filter_map utxos ~f:input_and_script_of_utxo in
    let inputs = List.map inputs_and_scripts ~f:fst in
    let change_out =
      output_of_dest_addr source_b58 ~value:0L in
    let outputs =
      change_out :: List.map dest_addrs ~f:(output_of_dest_addr ~value:0L) in
    let tx = Transaction.create inputs outputs in

    (* Compute fees *)

    let size = Transaction.serialized_size tx in
    let fees_per_byte = cfg.fees in
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
    let outputs = if change < fees then List.tl_exn outputs else outputs in
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
    eprintf "%s\n" (Transaction.show tx) ;
    printf "%s\n" tx_hex
  end

let spend_n loglevel cfg privkey tezos_addrs amount =
  let cfg = Cfg.unopt cfg in
  let amount =
    Option.map amount ~f:(fun a -> Int.of_float (Float.(a * 1e8))) in
  set_loglevel loglevel ;
  let tezos_addrs = match tezos_addrs with
    | [] -> List.map Stdio.In_channel.(input_lines stdin)
              ~f:Base58.Tezos.of_string_exn
    | _ -> tezos_addrs in
  Lwt_main.run begin
    spend_n cfg privkey tezos_addrs amount >|= function
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
  Term.(const spend_n $ loglevel $ cfg $ privkey $ tezos_addrs $ amount),
  Term.info ~doc "spend-n"

type tezos_addr_inputs = {
  amount : int ;
  prevtxs : string list ; (* binary raw txs *)
  inputs : Transaction.Input.t list ;
}

let group_utxos_by_address utxos =
  let groups = List.group utxos ~break:begin fun u u' ->
      not Base58.Bitcoin.(u.Utxo.address = u'.address)
    end in
  List.filter_map groups ~f:begin function
    | [] -> None
    | (u :: _) as group -> Some (u.Utxo.address, group)
  end

let payment_addrs_of_hashtbl htbl =
  let users = Hashtbl.Poly.data htbl in
  List.map users ~f:(fun u -> u.User.payment_address)

let get_rawtxs txids =
  let module SC = Set.Using_comparator in
  let empty = SC.empty ~comparator:String.comparator in
  let final = List.fold_left txids ~init:empty ~f:(fun a (`Hex txid) -> SC.add a txid) in
  let txids = List.map (SC.elements final) ~f:(fun txid -> `Hex txid) in
  let nb_txids = List.length txids in
  Lwt_log.debug_f
    "get_rawtxs: will do %d calls to Blockexplorer." nb_txids >>= fun () ->
  Lwt_list.filter_map_s begin fun txid ->
    Blockexplorer_lwt.rawtx txid >|= function
    | Ok rawtx -> Some rawtx
    | _ -> None
  end txids >>= fun rawtxs ->
  let nb_txids = List.length txids in
  let nb_rawtxs = List.length rawtxs in
  if List.(length rawtxs = length txids) then Lwt.return rawtxs
  else Lwt.fail_with
      (Printf.sprintf "get_rawtxs: only %d/%d calls succeeded" nb_rawtxs nb_txids)

let utxos_of_tezos_addrs ?min_confirmations ~cfg tezos_addrs =
  let fee_per_byte = cfg.Cfg.fees in
  let min_amount = tezos_input_size * fee_per_byte in
  let tezos_addr_to_user_record = Hashtbl.Poly.create () in
  let payment_addr_to_user_record = Hashtbl.Poly.create () in
  List.iter tezos_addrs ~f:begin fun tezos_addr ->
      let user = User.of_tezos_addr ~cfg tezos_addr in
      Hashtbl.Poly.set tezos_addr_to_user_record tezos_addr user ;
      Hashtbl.Poly.set payment_addr_to_user_record user.payment_address user
  end ;
  let payment_addresses = payment_addrs_of_hashtbl tezos_addr_to_user_record in
  Blockexplorer_lwt.utxos payment_addresses >>= function
  | Error err -> Lwt.return (Error err)
  | Ok utxos ->
    get_rawtxs (List.map utxos ~f:(fun u -> u.Utxo.txid)) >|= fun rawtxs ->
    let utxos = group_utxos_by_address utxos in
    Ok begin List.map utxos ~f:begin fun (payment_addr, utxos) ->
        let { User.scriptRedeem ; payment_address } =
          Hashtbl.find_exn payment_addr_to_user_record payment_addr in
        let amount, prevtxs, inputs =
          List.fold2_exn rawtxs utxos
            ~init:(0, [], []) ~f:begin fun (sum, prevtxs, inputs) rawtx utxo ->
            if utxo.amount <= min_amount then begin
              Lwt_log.ign_info_f "discard UTXO with amount %d sats, (fee %d sats/byte)"
                utxo.amount fee_per_byte ;
              sum, prevtxs, inputs
            end
            else
              let input_and_script =
                input_and_script_of_utxo ?min_confirmations ~script:scriptRedeem utxo in
              match input_and_script with
              | None -> sum, prevtxs, inputs
              | Some (input, _script) ->
                sum + utxo.amount, rawtx :: prevtxs, input :: inputs
          end
        in
        { amount ; prevtxs ; inputs }
      end
    end

let prepare_multisig_tx cfg min_confirmations tezos_addrs dest =
  let fee_per_byte = cfg.Cfg.fees in
  utxos_of_tezos_addrs ~min_confirmations ~cfg tezos_addrs >>= function
  | Error err ->
    Lwt_log.debug_f "prepare_multisig_tx: utxos_of_tezos_addrs" >>= fun () ->
    Lwt.return_none
  | Ok tezos_inputs ->
    let nb_utxos = List.fold_left tezos_inputs ~init:0 ~f:begin fun a { inputs } ->
        a + List.length inputs
      end in
    let total_amount =
      List.fold_left tezos_inputs ~init:0 ~f:(fun a { amount } -> a + amount) in
    Lwt_log.ign_debug_f "Selected %d utxos with total amount %d (%f)"
      nb_utxos total_amount (total_amount // 100_000_000) ;
    let output = output_of_dest_addr dest ~value:0L in
    let prevtxs = List.map tezos_inputs ~f:(fun { prevtxs } -> prevtxs) in
    let prevtxs = List.concat prevtxs in
    let inputs = List.map tezos_inputs ~f:(fun { inputs } -> inputs) in
    let tx = Transaction.create (List.concat inputs) [output] in
    let tx_size = Transaction.serialized_size tx in
    let spendable_amount = total_amount - tx_size * fee_per_byte in
    if spendable_amount <= 0 then
      Lwt.return_none
    else
      let output = output_of_dest_addr dest ~value:(Int64.of_int spendable_amount) in
      Transaction.set_outputs tx [output] ;
      Lwt.return_some (prevtxs, tx)

let prepare_multisig loglevel cfg min_confirmations tezos_addrs dest key_id =
  let cfg = Cfg.unopt cfg in
  let keyPath = Bip44.create ~index:key_id () in
  set_loglevel loglevel ;
  let tezos_addrs = match tezos_addrs with
    | [] -> List.map Stdio.In_channel.(input_lines stdin)
              ~f:Base58.Tezos.of_string_exn
    | _ -> tezos_addrs in
  Lwt_main.run begin
    prepare_multisig_tx cfg min_confirmations tezos_addrs dest >|= function
    | None -> Caml.exit 1
    | Some (prevtxs, tx) ->
      let `Hex tx_hex = Transaction.to_hex tx in
      let nb_inputs = List.length (Transaction.get_inputs tx) in
      if is_verbose loglevel then eprintf "Prepared transaction with %d inputs.\n" nb_inputs ;
      if is_debug loglevel then eprintf "%s\n" (Transaction.show tx) ;
      Caml.Format.printf "rawTx = \"%s\"@." tx_hex ;
      Caml.Format.printf "keyPath = \"%a\"@." Bip44.pp keyPath;
      Caml.Format.printf "rawPrevTxs = [ %a ]@." pp_print_quoted_string_list
        (List.map prevtxs ~f:(fun ptx -> let `Hex ptx_hex = Hex.of_string ptx in ptx_hex))
  end

let prepare_multisig =
  let doc = "Prepare a transaction for Ledger." in
  let min_confirmations =
    let doc = "Minimal number of confirmations required." in
    Arg.(value & (opt int 1) & info ~doc ["min-confirmations"]) in
  let dest =
    Arg.(required & (pos 0 (some Conv.payment_addr) None) & info [] ~docv:"DEST") in
  let tezos_addrs =
    Arg.(value & (pos_right 1 Conv.tezos_addr []) & info [] ~docv:"TEZOS_ADDRS") in
  let key_id =
    let doc = "Ledger key id (using default wallet)" in
    Arg.(value & (opt int 0) & info ~doc ["i" ; "key-id"]) in
  Term.(const prepare_multisig $ loglevel $ cfg $
        min_confirmations $ tezos_addrs $ dest $ key_id),
  Term.info ~doc "prepare-multisig"

let describe_multisig tx =
  let tx = match tx with
    | None -> In_channel.(input_all stdin)
    | Some tx -> In_channel.read_all tx in
  let tx = match Toml.Parser.from_string tx with
    | `Ok cfg -> cfg
    | `Error (msg, _) -> invalid_arg ("Toml: " ^ msg) in
  let tx = match TomlTypes.Table.find (Toml.key "rawTx") tx with
    | TString rawTx -> Hex.to_string (`Hex rawTx)
    | _ -> invalid_arg "Toml: rawTx is not a string" in
  let tx = Transaction.of_bytes_exn tx in
  let outputs = Transaction.get_outputs tx in
  List.iteri outputs ~f:begin fun i o ->
    let value = Transaction.Output.get_value o in
    let value = Float.(Int64.to_float value / 1e8) in
    let script = Transaction.Output.get_script o in
    match Script.operation script 2 with
    | None -> ()
    | Some operation ->
      let addr_bytes = Script.Operation.to_bytes operation in
      let addr_bytes = String.subo addr_bytes ~pos:1 in
      let addr = Base58.Bitcoin.create addr_bytes in
      Caml.Format.printf "Output %d: %f XBT to %a@."
        i value Base58.Bitcoin.pp addr ;
  end

let describe_multisig =
  let doc = "Describe a transaction in Ledger cfg format." in
  let tx =
    Arg.(value & (pos 0 (some file) None) & info [] ~docv:"LEDGER_TX") in
  Term.(const describe_multisig $ tx),
  Term.info ~doc "describe-multisig"

let finalize_multisig loglevel ed1 ed2 tx =
  let tx = match tx with
    | None -> In_channel.(input_all stdin)
    | Some tx -> tx in
  let tx = match Toml.Parser.from_string tx with
    | `Ok cfg -> cfg
    | `Error (msg, _) -> invalid_arg ("Toml: " ^ msg) in
  let tx = match TomlTypes.Table.find (Toml.key "rawTx") tx with
    | TString rawTx -> Hex.to_string (`Hex rawTx)
    | _ -> invalid_arg "Toml: rawTx is not a string" in
  let tx = Transaction.of_bytes_exn tx in
  let inputs = Transaction.get_inputs tx in
  let ed1s = In_channel.read_lines ed1 in
  let ed2s = In_channel.read_lines ed2 in
  let inputs = List.map3_exn inputs ed1s ed2s ~f:begin fun input ed1 ed2 ->
      let ed1 = Hex.to_string (`Hex ed1) in
      let ed2 = Hex.to_string (`Hex ed2) in
      let scriptRedeem = Transaction.Input.get_script input in
      let scriptSig = Script.P2SH_multisig.scriptSig
          ~endorsements:[ ed1 ; ed2 ] ~scriptRedeem in
      Transaction.Input.set_script input scriptSig ;
      input
    end in
  Transaction.set_inputs tx inputs ;
  let `Hex tx_hex = Transaction.to_hex tx in
  if is_verbose loglevel then eprintf "%s\n" (Transaction.show tx) ;
  printf "%s\n" tx_hex

let finalize_multisig =
  let doc = "Finalize a multisig transaction." in
  let endorsement_file i =
    Arg.(required & (pos i (some file) None) & info [] ~docv:"FILE") in
  let tx =
    Arg.(value & (pos 2 (some string) None) & info [] ~docv:"TX") in
  Term.(const finalize_multisig $ loglevel
        $ (endorsement_file 0) $ (endorsement_file 1) $ tx),
  Term.info ~doc "finalize-multisig"

let gen_config loglevel threshold pks =
  let cfg = Cfg.of_pks ~threshold ~fees:300 pks in
  printf "%s\n" (Cfg.to_string cfg)

let gen_config =
  let doc = "Generate configuration file." in
  let threshold =
    Arg.(required & (pos 0 (some int) None) & info [] ~docv:"INT") in
  let pks =
    Arg.(value & (pos_right 1 Conv.hex []) & info [] ~docv:"BTC_PUBLIC_KEY") in
  Term.(const gen_config $ loglevel $ threshold $ pks),
  Term.info ~doc "gen-config"

let post_mortem loglevel cfg fn size =
  set_loglevel loglevel ;
  let cfg = Cfg.unopt cfg in
  let tx1s = Stdio.In_channel.read_lines fn in
  let tx1s = List.map tx1s ~f:begin fun tz1 ->
      User.of_tezos_addr ~cfg (Base58.Tezos.of_string_exn tz1)
    end in
  let total_nb_tz1s = List.length tx1s in
  let tx1s = List.groupi tx1s ~break:(fun i _ _ -> i % size = 0) in
  let on_vout addrs ({ n; value;
                 spentTxId; spentIndex; spentHeight;
                 scriptPubKey = { addresses; asm; hex; typ } } : Tx.Vout.t) =
    let addrs' = Base58.Bitcoin.Set.of_list addresses in
    if not Base58.Bitcoin.Set.(is_empty (inter addrs addrs'))
       && value < 5_000_000 && value >= 1_000_000 then
      Caml.Format.printf "%a %f@."
        Base58.Bitcoin.pp (Base58.Bitcoin.Set.min_elt addrs')
        Float.(of_int value / 1e8)
  in
  let on_tx addrs ({ txid; version; isCoinbase; size;
                     time; locktime; confirmations; valueIn;
                     valueOut; fees; blockheight;
                     blocktime; blockhash; vin; vout } : Tx.t) =
    List.iter vout ~f:(on_vout addrs)
  in
  let on_batch i batch =
    let addrs = List.map batch ~f:begin fun { User.payment_address } ->
        payment_address
      end in
    let rec loop () =
      BL.all_tx_by_addrs addrs >>= function
      | Error err ->
        Lwt_log.error (Http.string_of_error err) >>= fun () ->
        Lwt_unix.sleep 5. >>=
        loop
      | Ok txs ->
        let nb_txs = List.length txs in
        Lwt_log.info_f
          "Processing batch %d (%d/%d TZ1s), %d txs found"
          i ((i+1)*size) total_nb_tz1s nb_txs >>= fun () ->
        let addrs = Base58.Bitcoin.Set.of_list addrs in
        List.iter txs ~f:(on_tx addrs) ;
        Lwt.return_unit in
    loop () in
  let run () = Lwt_list.iteri_s on_batch tx1s in
  Lwt_main.run (run ())

let post_mortem =
  let doc = "Find discarded contributions." in
  let size =
    let doc = "TZ1 batch size." in
    Arg.(value & (opt int 100) & info ~doc ["batch-size"]) in
  let fn =
    Arg.(required & (pos 0 (some file) None & info [] ~docv:"TX1_FILE")) in
  Term.(const post_mortem $ loglevel $ cfg $ fn $ size),
  Term.info ~doc "post-mortem"

let default_cmd =
  let doc = "Crowdsale tools." in
  Term.(ret (const (`Help (`Pager, None)))),
  Term.info ~doc "crowdsale"

let cmds = [
  gen_config ;

  prepare_multisig ;
  describe_multisig ;
  finalize_multisig ;

  post_mortem ;
]

let () = match Term.eval_choice default_cmd cmds with
  | `Error _ -> Caml.exit 1
  | #Term.result -> Caml.exit 0
