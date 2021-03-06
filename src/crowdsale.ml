open Base
open Stdio
open Rresult
open Cmdliner
open Lwt.Infix

open Blockexplorer
open Blockexplorer.Types

open Util
open Util.Cmdliner

let c = Bitcoin.Util.c

let tezos_input_size = 260 (* computed in the ocaml-libbitcoin testsuite *)

module User = struct
  type t = {
    tezos_addr : Base58.Tezos.t ;
    scriptRedeem : Bitcoin.Script.t ;
    payment_address : Base58.Bitcoin.t ;
  }

  let of_tezos_addr ~testnet ({ Base58.Tezos.version; payload } as tezos_addr) =
    let scriptRedeem = multisig_script (Cstruct.of_string payload) [pk1; pk2] in
    let payment_address = Bitcoin.Wallet.Address.of_script ~testnet scriptRedeem in
    { tezos_addr ; scriptRedeem ; payment_address }
end

let testnet_of_network = function
  | Blockexplorer_lwt.BTC_testnet | BCH_testnet -> true
  | _ -> false

let lookup_utxos loglevel network tezos_addrs =
  let testnet = testnet_of_network network in
  set_loglevel loglevel ;
  let tezos_addrs =
    if List.is_empty tezos_addrs then
      List.map In_channel.(input_lines stdin) ~f:(Base58.Tezos.of_string_exn c) else
      tezos_addrs in
  let run () =
    Lwt_log.debug_f "Found %d tezos addresses" (List.length tezos_addrs) >>= fun () ->
    let payment_addrs = List.map tezos_addrs ~f:begin fun tezos_addr ->
        let { User.payment_address } =
          User.of_tezos_addr ~testnet tezos_addr in
        payment_address
      end in
    List.iter2_exn tezos_addrs payment_addrs ~f:begin fun ta pa ->
      Lwt_log.ign_debug_f "%s -> %s" (Base58.Tezos.show c ta) (Base58.Bitcoin.show c pa)
    end ;
    Blockexplorer_lwt.utxos c ~network payment_addrs >>= function
    | Error err ->
      Lwt_log.error (Http.string_of_error err)
    | Ok utxos ->
      Lwt_log.debug_f "Blockexplorer: found %d utxo(s)" (List.length utxos) >|= fun () ->
      ignore begin List.iter2 payment_addrs utxos ~f:begin fun a u ->
          printf "%s\n" (Utxo.to_string c u)
        end
      end in
  Lwt_main.run (run ())

let lookup_utxos =
  let doc = "Get Bitcoin UTXOs from a Tezos address." in
  let tezos_addrs =
    Arg.(value & (pos_all (Conv.tezos_addr c) []) & info [] ~docv:"TEZOS_ADDR") in
  Term.(const lookup_utxos $ loglevel $ network $ tezos_addrs),
  Term.info ~doc "lookup-utxos"

let input_of_utxo
    ?(min_confirmations = 1)
    ?(script = [])
    utxo =
  match utxo.Utxo.confirmed with
  | Unconfirmed _ -> None
  | Confirmed { confirmations } when confirmations < min_confirmations -> None
  | Confirmed { vout ; scriptPubKey } ->
    let prev_out_hash = Bitcoin.Util.Hash256.of_hex_rpc utxo.Utxo.txid in
    let script = Hex.to_cstruct (`Hex scriptPubKey) in
    let script, _cs = Bitcoin.Script.of_cstruct script (Cstruct.len script) in
    Some
      (Bitcoin.Protocol.TxIn.create' ~prev_out_hash ~prev_out_i:vout ~script ())

let input_of_prev_tx (prev_tx : Bitcoin.Protocol.Transaction.t) index =
  let open Bitcoin.Protocol in
  let txout = List.nth_exn prev_tx.outputs index in
  let prev_out_hash = Transaction.hash256 prev_tx in
  TxIn.create' ~prev_out_hash ~prev_out_i:index ~script:txout.script ()

let amount_of_utxos =
  List.fold_left ~init:0 ~f:(fun acc utxo -> acc + utxo.Utxo.amount)

let spend_n network prev_txid prev_out path tezos_addrs =
  let open Bitcoin.Protocol in
  let testnet = match network with
    | Blockexplorer_lwt.BTC_testnet
    | BCH_testnet -> true
    | _ -> false in
  let h = Hidapi.open_id_exn ~vendor_id:0x2C97 ~product_id:0x0001 in
  let dest_addrs = List.map tezos_addrs ~f:begin fun addr ->
      let { User.payment_address } = User.of_tezos_addr ~testnet addr in
      payment_address
    end in
  Blockexplorer_lwt.rawtx ~network prev_txid >|=
  R.map begin fun prev_tx ->
    let prev_tx, _cs = Transaction.of_cstruct (Cstruct.of_string prev_tx) in
    let utxo = List.nth_exn prev_tx.outputs prev_out in

    (* Build transaction *)

    let nb_dests = List.length dest_addrs in
    let input = input_of_prev_tx prev_tx prev_out in
    let users = List.map tezos_addrs ~f:(User.of_tezos_addr ~testnet) in
    let outputs = List.map users ~f:begin fun { scriptRedeem } ->
        TxOut.create ~value:0L ~script:scriptRedeem
    end in

    (* Compute fees *)

    let tx = Transaction.create [input] outputs () in
    let size = Transaction.size tx in
    let fees = size * fees_per_byte in
    let spendable_amount = Int64.(utxo.value - of_int fees) in
    let amount_per_addr = Int64.(spendable_amount / of_int nb_dests) in

    let outputs = List.map outputs ~f:begin fun o ->
        { o with value = amount_per_addr }
      end in

    let tx = Transaction.create [input] outputs () in
    Caml.Format.printf "%a@." Transaction.pp tx ;
    let `Hex tx_hex = Transaction.to_hex tx in
    Caml.Format.printf "%s@." tx_hex ;
    (* Sign transaction *)

    let signatures =
      match network with
      | BCH | BCH_testnet ->
        let prev_amounts = [utxo.value] in
        Ledgerwallet.sign_segwit ~bch:true ~path ~prev_amounts h tx
      | _ ->
        Ledgerwallet.sign ~path ~prev_outputs:[prev_tx, prev_out] h tx
    in
    let signature = List.hd_exn signatures in
    let script =
      Bitcoin.Script.Std.P2PKH.(scriptSig ctx signature pk1 @ scriptRedeem ctx pk1) in
    let input = { input with script } in
    let tx = Transaction.create [input] outputs () in
    let cs = Cstruct.create (Transaction.size tx) in
    let _ = Transaction.to_cstruct cs tx in
    let `Hex tx_hex = Hex.of_cstruct cs in
    Caml.Format.printf "%a@." Transaction.pp tx ;
    printf "%s\n" tx_hex
  end

let spend_n loglevel network prev_txid prev_out path tezos_addrs =
  set_loglevel loglevel ;
  let prev_txid = Hex.of_string prev_txid in
  let tezos_addrs = match tezos_addrs with
    | [] -> List.map Stdio.In_channel.(input_lines stdin)
              ~f:(Base58.Tezos.of_string_exn c)
    | _ -> tezos_addrs in
  Lwt_main.run begin
    spend_n network prev_txid prev_out path tezos_addrs >|= function
    | Ok () -> ()
    | Error err -> Lwt_log.ign_error (Http.string_of_error err)
  end

let spend_n =
  let prev_txid =
    Arg.(required & (pos 0 (some Conv.hex) None) & info [] ~docv:"PREV_TXID") in
  let prev_out =
    Arg.(required & (pos 1 (some int) None) & info [] ~docv:"PREV_OUT") in
  let path =
    Arg.(required & (pos 2 (some Conv.path) None) & info [] ~docv:"PATH") in
  let tezos_addrs =
    Arg.(value & (pos_right 2 (Conv.tezos_addr c) []) & info [] ~docv:"TEZOS ADDRS") in
  let doc = "Spend bitcoins equally between n addresses." in
  Term.(const spend_n $ loglevel $ network $ prev_txid $ prev_out $ path $ tezos_addrs),
  Term.info ~doc "spend-n"

(* type tezos_addr_inputs = {
 *   amount : int ;
 *   prevtxs : string list ; (\* binary raw txs *\)
 *   inputs : Transaction.Input.t list ;
 * }
 * 
 * (\* let tezos_addr_inputs_encoding = *\)
 * (\*   let open Json_encoding in *\)
 * (\*   conv *\)
 * (\*     (fun { tezos_addr ; amount ; scriptRedeem ; inputs } -> *\)
 * (\*        let tezos_addr = Base58.Tezos.to_string tezos_addr in *\)
 * (\*        let `Hex scriptRedeem = Script.to_hex scriptRedeem in *\)
 * (\*        let inputs = List.map inputs *\)
 * (\*            ~f:(fun i -> let `Hex i_hex = Transaction.Input.to_hex i in i_hex) in *\)
 * (\*        (tezos_addr, amount, scriptRedeem, inputs)) *\)
 * (\*     (fun (tezos_addr, amount, scriptRedeem, inputs) -> *\)
 * (\*        let tezos_addr = Base58.Tezos.of_string_exn tezos_addr in *\)
 * (\*        let scriptRedeem = Script.of_hex_exn (`Hex scriptRedeem) in *\)
 * (\*        let inputs = List.map inputs *\)
 * (\*            ~f:(fun i_hex -> Transaction.Input.of_hex_exn (`Hex i_hex)) in *\)
 * (\*        { tezos_addr ; scriptRedeem ; amount ; inputs }) *\)
 * (\*     (obj4 *\)
 * (\*        (req "tezos_addr" string) *\)
 * (\*        (req "amount" int) *\)
 * (\*        (req "scriptRedeem" string) *\)
 * (\*        (req "inputs" (list string))) *\)
 * 
 * (\* type tx = { *\)
 * (\*   tx : Transaction.t ; *\)
 * (\*   inputs: tezos_addr_inputs list ; *\)
 * (\* } *\)
 * 
 * (\* let tx_encoding = *\)
 * (\*   let open Json_encoding in *\)
 * (\*   conv *\)
 * (\*     (fun { tx ; inputs } -> *\)
 * (\*        let `Hex tx_hex = Transaction.to_hex tx in (tx_hex, inputs)) *\)
 * (\*     (fun (tx, inputs) -> { tx = Transaction.of_hex_exn (`Hex tx) ; inputs }) *\)
 * (\*     (obj2 *\)
 * (\*        (req "tx" string) *\)
 * (\*        (req "inputs" (list tezos_addr_inputs_encoding))) *\)
 * 
 * let group_utxos_by_address utxos =
 *   let groups = List.group utxos ~break:begin fun u u' ->
 *       not Base58.Bitcoin.(u.Utxo.address = u'.address)
 *     end in
 *   List.filter_map groups ~f:begin function
 *     | [] -> None
 *     | (u :: _) as group -> Some (u.Utxo.address, group)
 *   end
 * 
 * let payment_addrs_of_hashtbl htbl =
 *   let users = Hashtbl.Poly.data htbl in
 *   List.map users ~f:(fun u -> u.User.payment_address)
 * 
 * let get_rawtxs ~testnet txids =
 *   let module SC = Set.Using_comparator in
 *   let empty = SC.empty ~comparator:String.comparator in
 *   let final = List.fold_left txids ~init:empty ~f:(fun a (`Hex txid) -> SC.add a txid) in
 *   let txids = List.map (SC.elements final) ~f:(fun txid -> `Hex txid) in
 *   let nb_txids = List.length txids in
 *   Lwt_log.debug_f
 *     "get_rawtxs: will do %d calls to Blockexplorer." nb_txids >>= fun () ->
 *   Lwt_list.filter_map_s begin fun txid ->
 *     Blockexplorer_lwt.rawtx ~testnet txid >|= function
 *     | Ok rawtx -> Some rawtx
 *     | _ -> None
 *   end txids >>= fun rawtxs ->
 *   let nb_txids = List.length txids in
 *   let nb_rawtxs = List.length rawtxs in
 *   if List.(length rawtxs = length txids) then Lwt.return rawtxs
 *   else Lwt.fail_with
 *       (Printf.sprintf "get_rawtxs: only %d/%d calls succeeded" nb_rawtxs nb_txids)
 * 
 * let utxos_of_tezos_addrs ?min_confirmations ~cfg ~testnet tezos_addrs =
 *   let fee_per_byte = cfg.Cfg.fees in
 *   let min_amount = tezos_input_size * fee_per_byte in
 *   let tezos_addr_to_user_record = Hashtbl.Poly.create () in
 *   let payment_addr_to_user_record = Hashtbl.Poly.create () in
 *   List.iter tezos_addrs ~f:begin fun tezos_addr ->
 *       let user = User.of_tezos_addr ~cfg ~testnet tezos_addr in
 *       Hashtbl.Poly.set tezos_addr_to_user_record tezos_addr user ;
 *       Hashtbl.Poly.set payment_addr_to_user_record user.payment_address user
 *   end ;
 *   let payment_addresses = payment_addrs_of_hashtbl tezos_addr_to_user_record in
 *   Blockexplorer_lwt.utxos ~testnet payment_addresses >>= function
 *   | Error err -> Lwt.return (Error err)
 *   | Ok utxos ->
 *     get_rawtxs ~testnet (List.map utxos ~f:(fun u -> u.Utxo.txid)) >|= fun rawtxs ->
 *     let utxos = group_utxos_by_address utxos in
 *     Ok begin List.map utxos ~f:begin fun (payment_addr, utxos) ->
 *         let { User.scriptRedeem ; payment_address } =
 *           Hashtbl.find_exn payment_addr_to_user_record payment_addr in
 *         let amount, prevtxs, inputs =
 *           List.fold2_exn rawtxs utxos
 *             ~init:(0, [], []) ~f:begin fun (sum, prevtxs, inputs) rawtx utxo ->
 *             if utxo.amount <= min_amount then begin
 *               Lwt_log.ign_info_f "discard UTXO with amount %d sats, (fee %d sats/byte)"
 *                 utxo.amount fee_per_byte ;
 *               sum, prevtxs, inputs
 *             end
 *             else
 *               let input_and_script =
 *                 input_and_script_of_utxo ?min_confirmations ~script:scriptRedeem utxo in
 *               match input_and_script with
 *               | None -> sum, prevtxs, inputs
 *               | Some (input, _script) ->
 *                 sum + utxo.amount, rawtx :: prevtxs, input :: inputs
 *           end
 *         in
 *         { amount ; prevtxs ; inputs }
 *       end
 *     end
 * 
 * let prepare_multisig_tx cfg testnet min_confirmations tezos_addrs dest =
 *   let fee_per_byte = cfg.Cfg.fees in
 *   utxos_of_tezos_addrs ~min_confirmations ~cfg ~testnet tezos_addrs >>= function
 *   | Error err ->
 *     Lwt_log.debug_f "prepare_multisig_tx: utxos_of_tezos_addrs" >>= fun () ->
 *     Lwt.return_none
 *   | Ok tezos_inputs ->
 *     let nb_utxos = List.fold_left tezos_inputs ~init:0 ~f:begin fun a { inputs } ->
 *         a + List.length inputs
 *       end in
 *     let total_amount =
 *       List.fold_left tezos_inputs ~init:0 ~f:(fun a { amount } -> a + amount) in
 *     Lwt_log.ign_debug_f "Selected %d utxos with total amount %d (%f)"
 *       nb_utxos total_amount (total_amount // 100_000_000) ;
 *     let output = output_of_dest_addr dest ~value:0L in
 *     let prevtxs = List.map tezos_inputs ~f:(fun { prevtxs } -> prevtxs) in
 *     let prevtxs = List.concat prevtxs in
 *     let inputs = List.map tezos_inputs ~f:(fun { inputs } -> inputs) in
 *     let tx = Transaction.create (List.concat inputs) [output] in
 *     let tx_size = Transaction.serialized_size tx in
 *     let spendable_amount = total_amount - tx_size * fee_per_byte in
 *     if spendable_amount <= 0 then
 *       Lwt.return_none
 *     else
 *       let output = output_of_dest_addr dest ~value:(Int64.of_int spendable_amount) in
 *       Transaction.set_outputs tx [output] ;
 *       Lwt.return_some (prevtxs, tx)
 * 
 * let pp_print_quoted_string ppf str =
 *   let open Caml.Format in
 *   fprintf ppf "\"%s\"" str
 * 
 * let pp_print_quoted_string_list ppf strs =
 *   let open Caml.Format in
 *   pp_print_list ~pp_sep:(fun ppf () -> pp_print_string ppf ", ")
 *     pp_print_quoted_string ppf strs
 * 
 * let prepare_multisig loglevel cfg testnet min_confirmations tezos_addrs dest key_id =
 *   let cfg = Cfg.unopt cfg in
 *   let keyPath = Bip44.create
 *       ~coin_type:(if testnet then Bitcoin_testnet else Bitcoin)
 *       ~index:key_id () in
 *   set_loglevel loglevel ;
 *   let tezos_addrs = match tezos_addrs with
 *     | [] -> List.map Stdio.In_channel.(input_lines stdin)
 *               ~f:Base58.Tezos.of_string_exn
 *     | _ -> tezos_addrs in
 *   Lwt_main.run begin
 *     prepare_multisig_tx cfg testnet min_confirmations tezos_addrs dest >|= function
 *     | None -> Caml.exit 1
 *     | Some (prevtxs, tx) ->
 *       let `Hex tx_hex = Transaction.to_hex tx in
 *       let nb_inputs = List.length (Transaction.get_inputs tx) in
 *       if is_verbose loglevel then eprintf "Prepared transaction with %d inputs.\n" nb_inputs ;
 *       if is_debug loglevel then eprintf "%s\n" (Transaction.show tx) ;
 *       Caml.Format.printf "rawTx = \"%s\"@." tx_hex ;
 *       Caml.Format.printf "keyPath = \"%a\"@." Bip44.pp keyPath;
 *       Caml.Format.printf "rawPrevTxs = [ %a ]@." pp_print_quoted_string_list
 *         (List.map prevtxs ~f:(fun ptx -> let `Hex ptx_hex = Hex.of_string ptx in ptx_hex))
 *   end
 * 
 * let prepare_multisig =
 *   let doc = "Prepare a transaction for Ledger." in
 *   let min_confirmations =
 *     let doc = "Minimal number of confirmations required." in
 *     Arg.(value & (opt int 1) & info ~doc ["min-confirmations"]) in
 *   let dest =
 *     Arg.(required & (pos 0 (some Conv.payment_addr) None) & info [] ~docv:"DEST") in
 *   let tezos_addrs =
 *     Arg.(value & (pos_right 1 Conv.tezos_addr []) & info [] ~docv:"TEZOS_ADDRS") in
 *   let key_id =
 *     let doc = "Ledger key id (using default wallet)" in
 *     Arg.(value & (opt int 0) & info ~doc ["i" ; "key-id"]) in
 *   Term.(const prepare_multisig $ loglevel $ cfg $
 *         testnet $ min_confirmations $ tezos_addrs $ dest $ key_id),
 *   Term.info ~doc "prepare-multisig"
 * 
 * let describe_multisig testnet tx =
 *   let tx = match tx with
 *     | None -> In_channel.(input_all stdin)
 *     | Some tx -> In_channel.read_all tx in
 *   let tx = match Toml.Parser.from_string tx with
 *     | `Ok cfg -> cfg
 *     | `Error (msg, _) -> invalid_arg ("Toml: " ^ msg) in
 *   let tx = match TomlTypes.Table.find (Toml.key "rawTx") tx with
 *     | TString rawTx -> Hex.to_string (`Hex rawTx)
 *     | _ -> invalid_arg "Toml: rawTx is not a string" in
 *   let tx = Transaction.of_bytes_exn tx in
 *   let outputs = Transaction.get_outputs tx in
 *   List.iteri outputs ~f:begin fun i o ->
 *     let value = Transaction.Output.get_value o in
 *     let value = Float.(Int64.to_float value / 1e8) in
 *     let script = Transaction.Output.get_script o in
 *     match Script.operation script 2 with
 *     | None -> ()
 *     | Some operation ->
 *       let addr_bytes = Script.Operation.to_bytes operation in
 *       let addr_bytes = String.subo addr_bytes ~pos:1 in
 *       let version = Base58.Bitcoin.(if testnet then Testnet_P2PKH else P2PKH) in
 *       let addr = Base58.Bitcoin.create ~version addr_bytes in
 *       Caml.Format.printf "Output %d: %f XBT to %a@."
 *         i value Base58.Bitcoin.pp addr ;
 *   end
 * 
 * let describe_multisig =
 *   let doc = "Describe a transaction in Ledger cfg format." in
 *   let tx =
 *     Arg.(value & (pos 0 (some file) None) & info [] ~docv:"LEDGER_TX") in
 *   Term.(const describe_multisig $ testnet $ tx),
 *   Term.info ~doc "describe-multisig"
 * 
 * let endorse_multisig loglevel cfg key_id tx =
 *   let cfg = Cfg.unopt cfg in
 *   let tx = match tx with
 *     | None -> Hex.to_string (`Hex In_channel.(input_line_exn stdin))
 *     | Some tx -> tx in
 *   let tx = Transaction.of_bytes_exn tx in
 *   let sk = List.nth_exn cfg.Cfg.sks key_id in
 *   let secret = Ec_private.secret sk in
 *   let endorsements = List.mapi (Transaction.get_inputs tx) ~f:begin fun index input ->
 *       let open Transaction.Sign in
 *       let prev_out_script = Transaction.Input.get_script input in
 *       Transaction.Sign.endorse_exn ~tx ~index ~prev_out_script ~secret ()
 *     end in
 *   List.iter endorsements ~f:begin fun e ->
 *     let `Hex e_hex = Hex.of_string e in
 *     printf "%s\n" e_hex
 *   end
 * 
 * let endorse_multisig =
 *   let doc = "Endorse a multisig transaction." in
 *   let key_id =
 *     let doc = "index of the key to use in the config file." in
 *     Arg.(value & (opt int 0) & info ["i" ; "key-id"] ~doc) in
 *   let tx =
 *     Arg.(value & (pos 0 (some Conv.hex) None) & info [] ~docv:"TX") in
 *   Term.(const endorse_multisig $ loglevel $ cfg $ key_id $ tx),
 *   Term.info ~doc "endorse-multisig"
 * 
 * let finalize_multisig loglevel ed1 ed2 tx =
 *   let tx = match tx with
 *     | None -> In_channel.(input_all stdin)
 *     | Some tx -> tx in
 *   let tx = match Toml.Parser.from_string tx with
 *     | `Ok cfg -> cfg
 *     | `Error (msg, _) -> invalid_arg ("Toml: " ^ msg) in
 *   let tx = match TomlTypes.Table.find (Toml.key "rawTx") tx with
 *     | TString rawTx -> Hex.to_string (`Hex rawTx)
 *     | _ -> invalid_arg "Toml: rawTx is not a string" in
 *   let tx = Transaction.of_bytes_exn tx in
 *   let inputs = Transaction.get_inputs tx in
 *   let ed1s = In_channel.read_lines ed1 in
 *   let ed2s = In_channel.read_lines ed2 in
 *   let inputs = List.map3_exn inputs ed1s ed2s ~f:begin fun input ed1 ed2 ->
 *       let ed1 = Hex.to_string (`Hex ed1) in
 *       let ed2 = Hex.to_string (`Hex ed2) in
 *       let scriptRedeem = Transaction.Input.get_script input in
 *       let scriptSig = Script.P2SH_multisig.scriptSig
 *           ~endorsements:[ ed1 ; ed2 ] ~scriptRedeem in
 *       Transaction.Input.set_script input scriptSig ;
 *       input
 *     end in
 *   Transaction.set_inputs tx inputs ;
 *   let `Hex tx_hex = Transaction.to_hex tx in
 *   if is_verbose loglevel then eprintf "%s\n" (Transaction.show tx) ;
 *   printf "%s\n" tx_hex
 * 
 * let finalize_multisig =
 *   let doc = "Finalize a multisig transaction." in
 *   let endorsement_file i =
 *     Arg.(required & (pos i (some file) None) & info [] ~docv:"FILE") in
 *   let tx =
 *     Arg.(value & (pos 2 (some string) None) & info [] ~docv:"TX") in
 *   Term.(const finalize_multisig $ loglevel
 *         $ (endorsement_file 0) $ (endorsement_file 1) $ tx),
 *   Term.info ~doc "finalize-multisig"
 * 
 * let spend_multisig cfg testnet min_confirmations tezos_addrs dest =
 *   prepare_multisig_tx cfg testnet min_confirmations tezos_addrs dest >|= function
 *   | None -> Caml.exit 1
 *   | Some (_, tx) ->
 *   let sk1, sk2 = match cfg.Cfg.sks with
 *     | sk1 :: sk2 :: _ -> sk1, sk2
 *     | _ -> failwith "Not enough sks" in
 *   let secret1, secret2 = Ec_private.(secret sk1, secret sk2) in
 *   let inputs =
 *     List.mapi (Transaction.get_inputs tx) ~f:begin fun index input ->
 *       let open Transaction.Sign in
 *       let prev_out_script = Transaction.Input.get_script input in
 *       let ed1 =
 *         Transaction.Sign.endorse_exn ~tx ~index ~prev_out_script ~secret:secret1 () in
 *       let ed2 =
 *         Transaction.Sign.endorse_exn ~tx ~index ~prev_out_script ~secret:secret2 () in
 *       let scriptSig =
 *         Script.P2SH_multisig.scriptSig
 *           ~endorsements:[ ed1 ; ed2 ]
 *           ~scriptRedeem:prev_out_script in
 *       Transaction.Input.set_script input scriptSig ;
 *       input
 *     end in
 *   Transaction.set_inputs tx inputs ;
 *   tx
 * 
 * let spend_multisig loglevel cfg testnet min_confirmations tezos_addrs dest =
 *   let cfg = Cfg.unopt cfg in
 *   set_loglevel loglevel ;
 *   let tezos_addrs = match tezos_addrs with
 *     | [] -> List.map Stdio.In_channel.(input_lines stdin)
 *               ~f:Base58.Tezos.of_string_exn
 *     | _ -> tezos_addrs in
 *   Lwt_main.run begin
 *     spend_multisig cfg testnet min_confirmations tezos_addrs dest >|= fun tx ->
 *     let `Hex tx_hex = Transaction.to_hex tx in
 *     if is_verbose loglevel then eprintf "%s\n" (Transaction.show tx) ;
 *     printf "%s\n" tx_hex
 *   end
 * 
 * let spend_multisig =
 *   let doc = "Spend bitcoins from a multisig address." in
 *   let min_confirmations =
 *     let doc = "Minimal number of confirmations required." in
 *     Arg.(value & (opt int 1) & info ~doc ["min-confirmations"]) in
 *   let dest =
 *     Arg.(required & (pos 0 (some Conv.payment_addr) None) & info [] ~docv:"DEST") in
 *   let tezos_addrs =
 *     Arg.(value & (pos_right 1 Conv.tezos_addr []) & info [] ~docv:"TEZOS_ADDRS") in
 *   Term.(const spend_multisig $ loglevel $ cfg $ testnet $ min_confirmations $ tezos_addrs $ dest),
 *   Term.info ~doc "spend-multisig"
 * 
 * let gen_config loglevel testnet pks =
 *   let cfg = Cfg.of_pks ~testnet pks in
 *   printf "%s\n" (Cfg.to_string cfg)
 * 
 * let gen_config =
 *   let doc = "Generate configuration file." in
 *   let pks =
 *     Arg.(value & (pos_all Conv.hex []) & info [] ~docv:"BTC_PUBLIC_KEY") in
 *   Term.(const gen_config $ loglevel $ testnet $ pks),
 *   Term.info ~doc "gen-config" *)

let default_cmd =
  let doc = "Crowdsale tools." in
  Term.(ret (const (`Help (`Pager, None)))),
  Term.info ~doc "crowdsale"

let cmds = [
  (* gen_config ; *)

  (* lookup_utxos ; *)
  spend_n ;
  (* spend_multisig ;
   * 
   * prepare_multisig ;
   * describe_multisig ;
   * endorse_multisig ;
   * finalize_multisig ; *)
]

let () = match Term.eval_choice default_cmd cmds with
  | `Error _ -> Caml.exit 1
  | #Term.result -> Caml.exit 0
