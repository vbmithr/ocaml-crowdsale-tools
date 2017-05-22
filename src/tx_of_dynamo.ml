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

module Http = struct
  open Caml
  type error =
    | Cohttp of exn
    | Client of string
    | Server of string
    | API of string
    | Data_encoding of string

  let api_str msg = API msg
  let api k =
    Format.kasprintf (fun msg -> API msg) k

  let api_fail msg = Error (API msg)

  let api_failf k =
    Format.kasprintf (fun msg -> Error (API msg)) k

  let data_encoding json = Error (Data_encoding json)

  let string_of_error = function
    | Cohttp exn -> Printexc.to_string exn
    | Client msg -> "HTTP Client error: " ^ msg
    | Server msg -> "HTTP Server error: " ^ msg
    | API msg -> "API error: " ^ msg
    | Data_encoding msg -> "Data encoding error: " ^ msg

  let pp_error ppf t =
    Format.fprintf ppf "%s" (string_of_error t)

  type nonrec 'a result = ('a, error) result
end

exception Client of string
exception Server of string
exception API of string
exception Data_encoding of string

let safe_get ?headers ~encoding url =
  Lwt.catch
    begin fun () ->
      Client.get ?headers url >>= fun (resp, body) ->
      let status_code = Cohttp.Code.code_of_status resp.status in
      Body.to_string body >|= fun body_str ->
      if Cohttp.Code.is_client_error status_code then
        raise (Client body_str)
      else if Cohttp.Code.is_server_error status_code then
        raise (Server body_str) ;
      let json = Ezjsonm.from_string body_str in
      try
        R.return (Json_encoding.destruct encoding json)
      with exn ->
        let pp = Json_encoding.print_error ?print_unknown:None in
        let str = Caml.Format.asprintf "%a" pp exn in
        raise (Data_encoding str)

    end
    begin function
      | Client str -> Lwt.return (R.fail (Http.Client str))
      | Server str -> Lwt.return (R.fail (Http.Server str))
      | API str -> Lwt.return (R.fail (Http.API str))
      | Data_encoding msg -> Lwt.return (R.fail (Http.Data_encoding msg))
      | exn -> Lwt.return (R.fail (Http.Cohttp exn))
    end

let safe_post ?(headers=Cohttp.Header.init ()) ~encoding url data =
  let headers =
    Cohttp.Header.add headers "content-type" "application/json" in
  let json =
    Json_encoding.construct encoding data in
  let pp_json ppf json = Json_repr.(pp ~compact:true (module Ezjsonm) ppf json) in
  let body = Body.of_string (Caml.Format.asprintf "%a" pp_json json) in
  Lwt.catch
    begin fun () ->
      Client.post ~headers ~body url >>= fun (resp, body) ->
      let status_code = Cohttp.Code.code_of_status resp.status in
      Body.to_string body >>= fun body_str ->
      Lwt_log.debug body_str >|= fun () ->
      if Cohttp.Code.is_client_error status_code then raise (Client body_str)
      else if Cohttp.Code.is_server_error status_code then raise (Server body_str) ;
      R.return `Null
      (* let json = Ezjsonm.from_string body_str in *)
      (* try *)
      (*   R.return (Json_encoding.destruct encoding json) *)
      (* with exn -> *)
      (*   let pp = Json_encoding.print_error ?print_unknown:None in *)
      (*   let str = Format.asprintf "%a" pp exn in *)
      (*   raise (Data_encoding str) *)
    end
    begin function
      | Client str -> Lwt.return (R.fail (Http.Client str))
      | Server str -> Lwt.return (R.fail (Http.Server str))
      | API str -> Lwt.return (R.fail (Http.API str))
      | Data_encoding msg -> Lwt.return (R.fail (Http.Data_encoding msg))
      | exn -> Lwt.return (R.fail (Http.Cohttp exn))
    end

module Utxo = struct
  type t = {
    tx : string ;
    vout : int ;
    tezos_address : Base58.Tezos.t ;
  }

  let encoding =
    let open Json_encoding in
    conv
      (fun { tx ; vout ; tezos_address } ->
         let `Hex tx_hex = Hex.of_string tx in
         ((), (tx_hex, vout, Base58.Tezos.to_string tezos_address)))
      (fun ((), (tx_hex, vout, tezos_addr)) ->
         let tx = Hex.to_string (`Hex tx_hex) in
         let tezos_address = Base58.Tezos.of_string_exn tezos_addr in
         { tx ; vout ; tezos_address })
      (merge_objs unit
         (obj3
            (req "tx" string)
            (req "vout" int)
            (req "tezos_address" string)))

  let pp ppf t =
    let json = Json_encoding.construct encoding t in
    Json_repr.(pp (module Ezjsonm) ppf json)

  let show t =
    Caml.Format.asprintf "%a" pp t
end

module Ack = struct
  type utxo = {
    txid : Hex.t ;
    vout : int ;
  }

  let utxo_encoding =
    let open Json_encoding in
    conv
      (fun { txid = `Hex txid; vout } ->
         (txid, vout))
      (fun (txid, vout) ->
         { txid = `Hex txid ; vout })
      (obj2
         (req "txId" string)
         (req "vout" int))

  type accepted = {
    timestamp : Ptime.t ;
    filename : string ;
  }

  let accepted_encoding =
    let open Json_encoding in
    conv
      (fun { timestamp ; filename } ->
         let timestamp = Ptime.to_float_s timestamp in
         (timestamp, filename))
      (fun (timestamp, filename) ->
         let timestamp = Option.value_exn (Ptime.of_float_s timestamp) in
         { timestamp ; filename })
      (obj2
         (req "timestamp" float)
         (req "filename" string))

  type meta =
    | Accepted of accepted
    | Rejected

  let meta_encoding =
    let open Json_encoding in
    union [
      case string
        (function Accepted _ -> None | _ -> Some "rejected")
        (fun _ -> Rejected) ;
      case accepted_encoding
        (function Accepted acc -> Some acc | _ -> None)
        (fun acc -> Accepted acc)
    ]

  type t = {
    utxos : utxo list ;
    meta : meta ;
  }

  let accept ~utxos ~filename =
    let timestamp = Ptime_clock.now () in
    let meta = Accepted { timestamp ; filename } in
    { utxos ; meta }

  let reject ~utxos = { utxos ; meta = Rejected }

  let encoding =
    let open Json_encoding in
    conv
      (fun { utxos ; meta } -> (utxos, meta))
      (fun (utxos, meta) -> { utxos ; meta })
      (obj2
         (req "utxos" (list utxo_encoding))
         (req "meta" meta_encoding))

  let pp ppf t =
    let json = Json_encoding.construct encoding t in
    Json_repr.(pp (module Ezjsonm) ppf json)

  let show t =
    Caml.Format.asprintf "%a" pp t

  let create ~txid ~vout = { txid ; vout }
end

module Input = struct
  type t = {
    tx : string ;
    input : Transaction.Input.t ;
    amount : Int64.t ;
    txid : Hex.t ;
    vout : int ;
  }

  let create ~tx ~input ~amount ~txid ~vout =
    { tx ; input ; amount ; txid ; vout }

  let to_ack { txid ; vout } = Ack.create ~txid ~vout
end

let inputs_of_utxos_scripts_exn cfg utxos scriptRedeems =
  let minimal_input_amount = Int64.of_int (cfg.Cfg.fees * tezos_input_size) in
  Lwt_log.ign_debug_f "Not including utxos with less than %Ld sats" minimal_input_amount ;
  let utxos_scripts = List.zip_exn utxos scriptRedeems in
  List.partition_map utxos_scripts ~f:begin fun ({ Utxo.tx = rawtx; vout }, scriptRedeem) ->
    let tx = Transaction.of_bytes_exn rawtx in
    Lwt_log.ign_debug_f "%s" (Transaction.show tx) ;
    let outputs = Transaction.get_outputs tx in
    let output = List.nth_exn outputs vout in
    let amount = Transaction.Output.get_value output in
    let txid = Hash.Hash32.to_hex tx.hash in
    match Int64.(amount <= minimal_input_amount) with
    | true -> `Snd (Ack.create ~txid ~vout)
    | false ->
      let input = Transaction.Input.create
          ~prev_out_hash:tx.hash ~prev_out_index:vout ~script:scriptRedeem () in
      `Fst (Input.create ~tx:rawtx ~input ~amount ~txid ~vout)
  end

let tx_of_inputs cfg inputs dest =
  let value =
    List.fold_left inputs ~init:0L ~f:(fun a { Input.amount } -> Int64.(a + amount)) in
  (* this should be a good approximation of fees *)
  let fees = cfg.Cfg.fees * (List.length inputs + 1) * tezos_input_size in
  let spendable_value = Int64.(value - (of_int fees)) in
  let script = Script.P2PKH.scriptPubKey dest in
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

let build_tx cfg loglevel max_size fn dest base_url =
  set_loglevel loglevel ;
  let cfg = Cfg.unopt cfg in
  let url = Uri.with_path base_url "getUtxos" in
  let url = Uri.with_query' url ["limit", Int.to_string max_size] in
  let encoding = Json_encoding.(list Utxo.encoding) in
  Lwt_log.debug_f "GET %s" (Uri.to_string url) >>= fun () ->
  safe_get ~encoding url >>= function
  | Error err -> Lwt.fail_with (Http.string_of_error err)
  | Ok utxos ->
    let scriptRedeems = List.map utxos ~f:begin fun { Utxo.tx ; vout ; tezos_address } ->
        let { User.scriptRedeem } = User.of_tezos_addr ~cfg tezos_address in
        scriptRedeem
      end in
    let big, small = inputs_of_utxos_scripts_exn cfg utxos scriptRedeems in
    begin match big with
      | [] -> Lwt.return_unit
      | inputs ->
        Lwt_log.info_f "Ack %d inputs" (List.length inputs) >>= fun () ->
        let tx = tx_of_inputs cfg big dest in
        output_ledger_cfg fn tx big ;
        let url = Uri.with_path base_url "ackUtxos" in
        let utxos = List.map big ~f:Input.to_ack in
        let resp = Ack.accept ~utxos ~filename:fn in
        safe_post ~encoding:Ack.encoding url resp >>= function
        | Ok `Null -> Lwt.return_unit
        | Error err ->
          Lwt_log.error_f "%s" (Http.string_of_error err)
    end >>= fun () ->
    begin match small with
      | [] -> Lwt.return_unit
      | utxos ->
        Lwt_log.info_f "Ignore %d inputs" (List.length utxos) >>= fun () ->
        let url = Uri.with_path base_url "ackUtxos" in
        let resp = Ack.reject ~utxos in
        safe_post ~encoding:Ack.encoding url resp >>= function
        | Ok `Null -> Lwt.return_unit
        | Error err ->
          Lwt_log.error_f "%s" (Http.string_of_error err)
    end

let build_tx cfg loglevel max_size fn dest base_url =
  Lwt_main.run (build_tx cfg loglevel max_size fn dest base_url)

let cmd =
  let max_size =
    let doc = "Maximum number of inputs for the transaction." in
    Arg.(value & (opt int 100) & info ~doc ["l" ; "limit"]) in
  let outf =
    Arg.(required & (pos 0 (some string) None & info [] ~docv:"OUT_FILE")) in
  let dest =
    Arg.(required & (pos 1 (some Conv.payment_addr) None & info [] ~docv:"DESTINATION_BTC_ADDRESS")) in
  let url =
    Arg.(required & (pos 2 (some Conv.uri) None & info [] ~docv:"URL")) in
  let doc = "Build a transaction from a Dynamo service." in
  Term.(const build_tx $ cfg $ loglevel $ max_size $ outf $ dest $ url),
  Term.info ~doc "tx_of_dynamo"

let () = match Term.eval cmd with
  | `Error _ -> Caml.exit 1
  | #Term.result -> Caml.exit 0
