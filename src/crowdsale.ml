open Core
open Async
open Log.Global

open Libbitcoin
open Blockexplorer
open Blockexplorer_async

open Util

let lookup loglevel cfg_file testnet () =
  set_level (loglevel_of_int loglevel) ;
  let version =
    Payment_address.(if testnet then Testnet_P2SH else P2SH) in
  let cfg =
    Option.value_map cfg_file ~default:Cfg.default ~f:begin fun fn ->
      Sexplib.Sexp.load_sexp_conv_exn fn Cfg.t_of_sexp
    end in
  let pks = List.filter_map cfg.pks ~f:(fun pk -> Ec_public.of_hex (`Hex pk)) in
  stage begin fun `Scheduler_started ->
    let pkhs = Reader.(lines (Lazy.force_val stdin)) in
    let addrs = Pipe.filter_map pkhs ~f:begin fun data ->
      Script.create_multisig ~data ~threshold:cfg.threshold pks |>
      Payment_address.of_script ~version |>
      Option.map ~f:Payment_address.to_b58check
      end in
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
  in
  Command.Staged.async
    ~summary:"Get Bitcoin UTXOs from a Tezos public key hash" spec lookup

let broadcast loglevel cfg_file testnet rawtx () =
  stage begin fun `Scheduler_started ->
    broadcast_tx ~testnet (`Hex rawtx) >>| function
    | Ok (`Hex txid) -> info "%s" txid
    | Error err -> info "%s" (Http.string_of_error err)
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
    | Ok t -> begin
        match Transaction.of_hex t with
        | None -> error "Unable to decode raw transaction"
        | Some tx ->
            let tx_decoded = Format.asprintf "%a" Transaction.pp tx in
            info "%s" tx_decoded
      end
    | Error err -> info "%s" (Http.string_of_error err)
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

let command =
  Command.group ~summary:"Crowdsale tools" [
    "fetch-tx", fetch ;
    "lookup-utxos", lookup ;
    "broadcast-tx", broadcast ;
  ]

let () = Command.run command

