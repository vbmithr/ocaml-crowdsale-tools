open Core
open Async
open Log.Global

open Libbitcoin
open Blockexplorer
open Blockexplorer_async

open Util

let main loglevel cfg_file testnet () =
  set_level (loglevel_of_int loglevel) ;
  let version =
    Payment_address.(if testnet then Testnet_P2SH else P2SH) in
  let cfg =
    Option.value_map cfg_file ~default:Cfg.default ~f:begin fun fn ->
      Sexplib.Sexp.load_sexp_conv_exn fn Cfg.t_of_sexp
    end in
  let pks = List.filter_map cfg.pks ~f:Ec_public.of_base16 in
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

let command =
  let spec =
    let open Command.Spec in
    empty
    +> flag "-loglevel" (optional_with_default 1 int) ~doc:"1-3 global loglevel"
    +> flag "-cfg" (optional file) ~doc:"filename Configuration file"
    +> flag "-testnet" no_arg ~doc:" Use bitcoin testnet"
  in
  Command.Staged.async ~summary:"Get utxos from pkhashes" spec main

let () = Command.run command
