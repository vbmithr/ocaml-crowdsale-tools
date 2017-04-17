open Core
open Async
open Log.Global

open Libbitcoin
open Blockexplorer
open Blockexplorer_async

open Util

let payment_addr_of_pkh_hex cfg version pks pkh_hex =
  let data = Hex.to_string (`Hex pkh_hex) in
  Script.create_multisig ~data ~threshold:cfg.Cfg.threshold pks |>
  Payment_address.of_script ~version |>
  Option.map ~f:Payment_address.to_b58check

let lookup loglevel cfg_file testnet pkhs () =
  set_level (loglevel_of_int loglevel) ;
  let version =
    Payment_address.(if testnet then Testnet_P2SH else P2SH) in
  let cfg =
    Option.value_map cfg_file ~default:Cfg.default ~f:begin fun fn ->
      Sexplib.Sexp.load_sexp_conv_exn fn Cfg.t_of_sexp
    end in
  let pks =
    List.filter_map cfg.pks ~f:(fun pk -> Ec_public.of_hex (`Hex pk)) in
  stage begin fun `Scheduler_started ->
    let pkhs =
      if pkhs = [] then
        Reader.(lines (Lazy.force_val stdin))
      else Pipe.of_list pkhs in
    let addrs = Pipe.filter_map pkhs
        ~f:(payment_addr_of_pkh_hex cfg version pks) in
    Pipe.to_list addrs >>= fun addrs ->
    utxos ~testnet addrs >>= function
    | Error err ->
        error "%s" (Http.string_of_error err) ;
        Scheduler.yield_until_no_jobs_remain () >>= fun () ->
        Shutdown.exit 1
    | Ok utxos ->
        let _ret = List.iter2 addrs utxos ~f:begin fun a u ->
            printf "%s" (Utxo.to_string u)
          end in
        Scheduler.yield_until_no_jobs_remain ()
  end

let lookup =
  let spec =
    let open Command.Spec in
    empty
    +> flag "-loglevel" (optional_with_default 1 int) ~doc:"1-3 global loglevel"
    +> flag "-cfg" (optional file) ~doc:"filename Configuration file"
    +> flag "-testnet" no_arg ~doc:" Use bitcoin testnet"
    +> anon (sequence ("pkh" %: string))
  in
  Command.Staged.async
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

let command =
  Command.group ~summary:"Crowdsale tools" [
    "decode-tx", decode_tx ;
    "decode-script", decode_script ;
    "fetch-tx", fetch ;
    "lookup-utxos", lookup ;
    "broadcast-tx", broadcast ;
  ]

let () = Command.run command

