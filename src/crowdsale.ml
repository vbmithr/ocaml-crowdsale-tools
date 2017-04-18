open Core
open Async
open Log.Global

open Libbitcoin
open Blockexplorer
open Blockexplorer_async

open Util

let cfg_of_cfg_file cfg_file =
  Option.value_map cfg_file
    ~default:Cfg.default
    ~f:(fun fn -> Sexplib.Sexp.load_sexp_conv_exn fn Cfg.t_of_sexp)

let script_and_payment_addr_of_pkh ~cfg ~testnet pkh =
  let version =
    Payment_address.(if testnet then Testnet_P2SH else P2SH) in
  let data = Hex.to_string pkh in
  let pks =
    List.filter_map cfg.Cfg.pks ~f:(fun pk -> Ec_public.of_hex (`Hex pk)) in
  let script =
    Script.create_multisig ~data ~threshold:cfg.threshold pks in
  let payment_address = Payment_address.of_script ~version script in
  Option.map payment_address
    ~f:(fun addr -> script, Payment_address.to_b58check addr)

let lookup loglevel cfg_file testnet pkhs () =
  set_level (loglevel_of_int loglevel) ;
  let cfg = cfg_of_cfg_file cfg_file in
  begin
    if pkhs = [] then
      Reader.(lines (Lazy.force_val stdin)) |> Pipe.to_list
    else Deferred.return pkhs
  end >>= fun pkhs ->
  debug "Found %d pkhs" (List.length pkhs) ;
  let addrs = List.filter_map pkhs
      ~f:(fun pkh -> script_and_payment_addr_of_pkh ~cfg ~testnet (`Hex pkh)) in
  List.iter2_exn pkhs addrs ~f:begin fun pkh (_script, addr) ->
    debug "%s -> %s" pkh addr
  end ;
  utxos ~testnet (List.map addrs ~f:snd) >>= function
  | Error err ->
      error "%s" (Http.string_of_error err) ;
      flushed ()
  | Ok utxos ->
      debug "Blockexplorer: found %d utxo(s)" (List.length utxos) ;
      ignore @@ List.iter2 addrs utxos ~f:begin fun a u ->
        printf "%s" (Utxo.to_string u)
      end ;
      flushed ()

let lookup =
  let spec =
    let open Command.Spec in
    empty
    +> flag "-loglevel" (optional_with_default 1 int) ~doc:"1-3 global loglevel"
    +> flag "-cfg" (optional file) ~doc:"filename Configuration file"
    +> flag "-testnet" no_arg ~doc:" Use bitcoin testnet"
    +> anon (sequence ("pkh" %: string))
  in
  Command.async
    ~summary:"Get Bitcoin UTXOs from a Tezos public key hash" spec lookup

let broadcast loglevel cfg_file testnet rawtx () =
  stage begin fun `Scheduler_started ->
    broadcast_tx ~testnet (`Hex rawtx) >>| function
    | Ok (`Hex txid) -> info "%s" txid
    | Error err -> error "%s" (Http.string_of_error err)
  end

let broadcast =
  let spec =
    let open Command.Spec in
    empty
    +> flag "-loglevel" (optional_with_default 1 int) ~doc:"1-3 global loglevel"
    +> flag "-cfg" (optional file) ~doc:"filename Configuration file"
    +> flag "-testnet" no_arg ~doc:" Use bitcoin testnet"
    +> anon ("rawtx" %: string)
  in
  Command.Staged.async
    ~summary:"Broadcast a transaction with blockexplorer.com API"
    spec broadcast

let fetch loglevel cfg_file testnet txid () =
  stage begin fun `Scheduler_started ->
    rawtx ~testnet (`Hex txid) >>| function
    | Error err -> error "%s" (Http.string_of_error err)
    | Ok t -> begin
        match Transaction.of_hex t with
        | None -> error "Unable to decode raw transaction"
        | Some tx ->
            let tx_decoded = Format.asprintf "%a" Transaction.pp tx in
            info "%s" tx_decoded
      end
  end

let fetch =
  let spec =
    let open Command.Spec in
    empty
    +> flag "-loglevel" (optional_with_default 1 int) ~doc:"1-3 global loglevel"
    +> flag "-cfg" (optional file) ~doc:"filename Configuration file"
    +> flag "-testnet" no_arg ~doc:" Use bitcoin testnet"
    +> anon ("txid" %: string)
  in
  Command.Staged.async
    ~summary:"Fetch a transaction from blockexplorer.com API"
    spec fetch

let decode_tx loglevel rawtx () =
  set_level (loglevel_of_int loglevel) ;
  match Transaction.of_hex (`Hex rawtx) with
  | None -> prerr_endline "Unable to decode"
  | Some tx -> Format.printf "%a@." Transaction.pp tx

let decode_tx =
  let spec =
    let open Command.Spec in
    empty
    +> flag "-loglevel" (optional_with_default 1 int) ~doc:"1-3 global loglevel"
    +> anon ("rawtx" %: string)
  in
  Command.basic
    ~summary:"Decode and print a transaction in raw format"
    spec decode_tx

let decode_script loglevel rawscript () =
  set_level (loglevel_of_int loglevel) ;
  match Script.of_hex (`Hex rawscript) with
  | None -> prerr_endline "Unable to decode"
  | Some script -> Format.printf "%a@." Script.pp script

let decode_script =
  let spec =
    let open Command.Spec in
    empty
    +> flag "-loglevel" (optional_with_default 1 int) ~doc:"1-3 global loglevel"
    +> anon ("rawscript" %: string)
  in
  Command.basic
    ~summary:"Decode and print a script in raw format"
    spec decode_script

let spend_n loglevel cfg_file testnet privkey source pkhs () =
  let pkhs = List.map pkhs ~f:Hex.of_string in
  let cfg = cfg_of_cfg_file cfg_file in
  let input_of_utxo utxo =
    let prev_out_hash = Hash.Hash32.of_hex_exn (`Hex utxo.Utxo.txid) in
    let prev_out_index = utxo.vout in
    match Script.of_hex (`Hex utxo.scriptPubKey) with
    | None -> invalid_arg "invalid script in source utxo"
    | Some script ->
        Transaction.Input.create ~prev_out_hash ~prev_out_index ~script ()
  in
  let amount_of_utxos =
    List.fold_left ~init:0 ~f:(fun acc utxo -> acc + utxo.Utxo.amount) in
  let output_of_pkh pkh ~value =
    match script_and_payment_addr_of_pkh ~cfg ~testnet pkh with
    | None -> invalid_arg "output_of_pkh"
    | Some (script, _addr) -> Transaction.Output.create ~script ~value in
  let endorsement_of_input tx secret index input =
    match Transaction.Sign.endorse
      ~prev_out_script:input.Transaction.Input.script
      ~tx ~index ~secret () with
    | None -> invalid_arg "endorsement_of_input"
    | Some endorsement -> endorsement in
  set_level (loglevel_of_int loglevel) ;
  let privkey = Ec_private.of_wif_exn privkey in
  let pubkey = Ec_public.of_private privkey in
  let secret = Ec_private.secret privkey in
  utxos ~testnet [source] >>| function
  | Error err -> error "%s" (Http.string_of_error err)
  | Ok utxos ->
      List.iter utxos ~f:begin fun utxo ->
        debug "%s" (Utxo.to_string utxo)
      end ;
      let nb_dests = List.length pkhs in
      let total_amount = amount_of_utxos utxos in
      debug "Found %d utxos with total amount %d (%f)"
        (List.length utxos) total_amount (total_amount // 100_000_000) ;
      let total_amount_1percent = total_amount / 100 in
      let total_spendable = total_amount - total_amount_1percent in
      let amount_per_pkh = Int64.of_int (total_spendable / nb_dests) in
      let inputs = List.map utxos ~f:input_of_utxo in
      let outputs = List.map pkhs ~f:(output_of_pkh ~value:amount_per_pkh) in
      let tx = Transaction.create inputs outputs in
      let endorsement_scripts = List.mapi inputs ~f:begin fun i input ->
          let open Transaction.Sign in
          let Endorsement chunk = endorsement_of_input tx secret i input in
          Script.endorsement chunk pubkey
        end in
      List.iter2_exn inputs endorsement_scripts ~f:begin fun i e ->
        Transaction.Input.set_script i e
      end ;
      Transaction.set_inputs tx inputs ;
      let `Hex tx_hex = Transaction.to_hex tx in
      printf "%s" (Transaction.show tx) ;
      printf "%s" tx_hex

let spend_n =
  let spec =
    let open Command.Spec in
    empty
    +> flag "-loglevel" (optional_with_default 1 int) ~doc:"1-3 global loglevel"
    +> flag "-cfg" (optional file) ~doc:"filename Configuration file"
    +> flag "-testnet" no_arg ~doc:" Use bitcoin testnet"
    +> anon ("privkey" %: string)
    +> anon ("source" %: string)
    +> anon (sequence ("dest" %: string))
  in
  Command.async
    ~summary:"Spend bitcoins equally between n addresses"
    spec spend_n

let command =
  Command.group ~summary:"Crowdsale tools" [
    "spend-n", spend_n ;
    "decode-tx", decode_tx ;
    "decode-script", decode_script ;
    "fetch-tx", fetch ;
    "lookup-utxos", lookup ;
    "broadcast-tx", broadcast ;
  ]

let () = Command.run command

