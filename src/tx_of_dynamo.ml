open Base
open Stdio
open Rresult
open Cmdliner
open Lwt.Infix
open Cohttp_lwt_unix
module Body = Cohttp_lwt_body
open Libbitcoin

open Util
open Util.Cmdliner

module Utxo = struct
  type t = {
    txid : string ;
    vout : int ;
    tezos_address : Base58.Tezos.t ;
    amount : int64 ;
  }

  let encoding =
    let open Json_encoding in
    conv
      (fun { txid ; vout ; tezos_address ; amount } ->
         let `Hex tx_hex = Hex.of_string txid in
         (tx_hex, vout, Base58.Tezos.to_string tezos_address, Int64.to_float amount))
      (fun (tx_hex, vout, tezos_addr, amount) ->
         let txid = Hex.to_string (`Hex tx_hex) in
         let tezos_address = Base58.Tezos.of_string_exn tezos_addr in
         let amount = Int64.of_float amount in
         { txid ; vout ; tezos_address ; amount })
      (obj4
         (req "txId" string)
         (req "vout" int)
         (req "tezos_address" string)
         (req "satoshis" float))

  let pp ppf t =
    let json = Json_encoding.construct encoding t in
    Json_repr.(pp (module Ezjsonm) ppf json)

  let show t =
    Caml.Format.asprintf "%a" pp t
end

module Input = struct
  type t = {
    tx : string ;
    input : Transaction.Input.t ;
    utxo : Utxo.t ;
  }

  let create ~tx ~input ~utxo =
    { tx ; input ; utxo }

end

let select_utxos cfg utxos max_amount max_size min_input =
  let minimal_input_amount = Int64.of_int (cfg.Cfg.fees * tezos_input_size) in
  let min_input = Int64.max minimal_input_amount min_input in
  Lwt_log.ign_debug_f
    "Not including utxos with less than %Ld sats" min_input ;
  (* Use larger utxos first *)
  let utxos = List.sort utxos
      ~cmp:(fun utxo1 utxo2 -> Int64.compare utxo2.Utxo.amount utxo1.Utxo.amount) in
  let rec select cur_total size unspent spent = function
    | [] -> Lwt.return (cur_total, size, List.rev unspent, List.rev spent)
    | ({ Utxo.txid ; vout ; tezos_address ; amount } as utxo) :: utxos ->
      if size >= max_size || Int64.(cur_total = max_amount) then
        Lwt.return (cur_total, size, List.rev unspent @ utxo :: utxos, List.rev spent)
      else if Int64.(amount < min_input) then
          select cur_total size (utxo :: unspent) spent utxos
      else
        let new_total = Int64.(cur_total + amount) in
        let new_size = size + 1 in
        if Int64.(new_total > max_amount) then
          select cur_total size (utxo :: unspent) spent utxos
        else
          Blockexplorer_lwt.rawtx (Hex.of_string txid) >>= fun rawtx ->
          let rawtx = match rawtx with
            | Ok rawtx -> rawtx
            | Error err ->
              Caml.Format.eprintf "%a@'" Blockexplorer.Http.pp_error err ;
              Caml.exit 2 in
          let tx = Transaction.of_bytes_exn rawtx in
          Lwt_log.ign_debug_f "%s" (Transaction.show tx) ;
          let outputs = Transaction.get_outputs tx in
          let output = List.nth_exn outputs vout in
          let actual_amount = Transaction.Output.get_value output in
          assert Int64.(actual_amount = amount) ;
          let { User.scriptRedeem } =
            User.of_tezos_addr ~cfg tezos_address in
          let input =
            Transaction.Input.create
              ~prev_out_hash:tx.hash ~prev_out_index:vout ~script:scriptRedeem () in
          let input =
            Input.create ~tx:rawtx ~input ~utxo in
          let `Hex txid = Hex.of_string txid in
          Caml.Format.eprintf "Included %s (%.2f BTC)@." txid Caml.(Int64.to_float amount /. 100000000.) ;
          select new_total new_size unspent (input :: spent) utxos
  in
  select Int64.zero 0 [] [] utxos

let tx_of_inputs cfg inputs dest =
  let value =
    List.fold_left inputs ~init:0L ~f:(fun a { Input.utxo = { amount } } -> Int64.(a + amount)) in
  (* this should be a good approximation of fees *)
  let fees = cfg.Cfg.fees * (List.length inputs + 1) * tezos_input_size in
  let spendable_value = Int64.(value - (of_int fees)) in
  let script = Payment_address.(to_script (of_b58check_exn dest)) in
  let output = Transaction.Output.create ~value:spendable_value ~script in
  let tx_inputs = List.map inputs ~f:(fun { Input.input } -> input) in
  Transaction.create tx_inputs [output]

let output_ledger_cfg fn tx inputs =
  let open Out_channel in
  let rawtxs = List.map inputs ~f:(fun { Input.tx } -> tx) in
  let `Hex tx_hex = Transaction.to_hex tx in
  Stdio.Out_channel.with_file ~binary:false ~append:false ~fail_if_exists:true fn ~f:begin fun oc ->
    fprintf oc "rawTx = \"%s\"\n" tx_hex ;
    output_string oc "keyPath = \"44'/0'/0'/0/0\"\n" ;
    let prevtxs_str =
      Caml.Format.asprintf "rawPrevTxs = [ %a ]@." pp_print_quoted_string_list
        (List.map rawtxs ~f:(fun ptx -> let `Hex ptx_hex = Hex.of_string ptx in ptx_hex)) in
    output_string oc prevtxs_str
  end

exception HTTP_Error
exception No_inputs_to_ack

let read_utxos fn =
  Lwt_io.with_file ~mode:Lwt_io.Input fn
    (fun chan -> Lwt_stream.to_list (Lwt_io.read_lines chan)) >>= fun utxos ->
  let utxos =
    List.map utxos (fun line ->
        let json = Ezjsonm.from_string line in
        Json_encoding.destruct Utxo.encoding json) in
  Lwt.return utxos

let write_utxos fn utxos =
  let utxos =
    List.map utxos (fun utxo ->
        let json = Json_encoding.construct Utxo.encoding utxo in
        let json = match json with `O fs -> `O fs | _ -> assert false in
        Ezjsonm.to_string ~minify: true json) in
  Lwt_io.with_file ~mode:Lwt_io.Output fn
    (fun chan -> Lwt_io.write_lines chan (Lwt_stream.of_list utxos))

let build_tx cfg loglevel min_size max_size fn dest max_amount min_input fn_unspent fn_spent =
  set_loglevel loglevel ;
  let cfg = Cfg.unopt cfg in
  Lwt.catch begin fun () ->
    read_utxos fn_unspent >>= fun utxos ->
    select_utxos cfg utxos max_amount max_size min_input >>= fun (total_amount, size, unspent, spent) ->
    if List.length spent < min_size then begin
      Lwt_log.info "Not enough inputs for tx. Exiting." >>= fun () ->
      Caml.exit 1
    end else begin
      Caml.Format.printf "Satoshis: %s@." (Int64.to_string total_amount);
      let tx = tx_of_inputs cfg spent dest in
      output_ledger_cfg fn tx spent ;
      read_utxos fn_spent >>= fun previous_spent ->
      let spent = List.map spent (fun { Input.utxo } -> utxo) in
      write_utxos fn_spent (spent @ previous_spent) >>= fun () ->
      write_utxos fn_unspent unspent >>= fun () ->
      Lwt.return ()
    end
  end
    begin function
    | HTTP_Error -> Caml.exit 1
    | No_inputs_to_ack -> Caml.exit 2
    | exn -> Caml.exit 3
  end

let build_tx cfg loglevel min_size max_size fn dest max_amount min_input unspent spent =
  Lwt_main.run (build_tx cfg loglevel min_size max_size fn dest max_amount min_input unspent spent)

let cmd =
  let min_size =
    let doc = "Minimum number of inputs for the transaction." in
    Arg.(value & (opt int 50) & info ~doc ["min" ; "min-inputs"]) in
  let max_size =
    let doc = "Maximum number of inputs for the transaction." in
    Arg.(value & (opt int 75) & info ~doc ["max" ; "max-inputs"]) in
  let min_input =
    let doc = "Minimum satoshis per input." in
    Arg.(value & (opt int64 5000000L) & info ~doc ["min-satoshi-per-input"]) in
  let outf =
    Arg.(required & (pos 0 (some string) None & info [] ~docv:"OUT_FILE")) in
  let dest =
    Arg.(required & (pos 1 (some Conv.payment_addr) None & info [] ~docv:"DESTINATION_BTC_ADDRESS")) in
  let max_amount =
    Arg.(required & (pos 2 (some int64) None) & info [] ~docv:"SATOSHIS_AMOUNT") in
  let unspent =
    Arg.(required & (pos 3 (some string) None & info [] ~docv:"UNSPENT_OUTPUTS_FILE")) in
  let spent =
    Arg.(required & (pos 4 (some string) None & info [] ~docv:"SPENT_OUTPUTS_FILE")) in
  let doc = "Build a transaction from a Dynamo service." in
  Term.(const build_tx $ cfg $ loglevel $ min_size $ max_size $ outf $ dest $ max_amount $ min_input $ unspent $ spent),
  Term.info ~doc "tx_of_dynamo"

let () = match Term.eval cmd with
  | `Error _ -> Caml.exit 10
  | #Term.result -> Caml.exit 0
