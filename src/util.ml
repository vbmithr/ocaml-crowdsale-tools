open Base
open Libbitcoin

let tezos_input_size = 260 (* computed in the ocaml-libbitcoin testsuite *)

module Cfg = struct
  let default_location =
    Caml.Filename.concat (Unix.getenv "HOME") ".tezos-crowdsale"

  type t = {
    pks : Ec_public.t list ;
    addrs : Payment_address.t list ;
    threshold : int ;
    fees : int ;
  }

  let of_pks ~threshold ~fees pks =
    let pks = List.map pks ~f:Ec_public.of_bytes_exn in
    let addrs = List.map pks ~f:Payment_address.of_point in
    { pks ; addrs ; threshold ; fees }

  let encoding =
    let open Json_encoding in
    conv
      begin fun { pks ; addrs ; threshold ; fees } ->
        let pks = List.map pks
            ~f:(fun pk -> let `Hex pk_hex = Ec_public.to_hex pk in pk_hex) in
        ((), (pks, threshold, fees))
      end
      begin fun ((), (pks, threshold, fees)) ->
        let pks = List.map pks ~f:(fun pk -> Ec_public.of_hex_exn (`Hex pk)) in
        let addrs =
          List.map pks ~f:Payment_address.of_point in
        { pks ; addrs ; threshold ; fees }
      end
      (merge_objs unit
      (obj3
         (req "pks" (list string))
         (req "threshold" int)
         (req "fees" int)))

  let of_file_exn fn =
    let json = Ezjsonm.from_channel (Stdio.In_channel.create fn) in
    Json_encoding.destruct encoding json

  let of_file fn =
    try Ok (of_file_exn fn) with exn ->
      Error (Caml.Format.asprintf "Cfg.of_file: %a"
               (Json_encoding.print_error ?print_unknown:None) exn)

  let to_ezjsonm cfg =
    Json_encoding.construct encoding cfg

  let pp ppf cfg =
    Json_repr.(pp ~compact:false (module Ezjsonm) ppf (to_ezjsonm cfg))

  let to_string cfg =
    Caml.Format.asprintf "%a" pp cfg

  let unopt = function
    | None -> of_file_exn default_location
    | Some cfg -> cfg
end

let set_loglevel vs =
  let level = match List.length vs with
    | 0 -> Lwt_log.Notice
    | 1 -> Info
    | _ -> Debug in
  Lwt_log.add_rule "*" level

let is_verbose vs = List.length vs > 0
let is_debug vs = List.length vs > 1

module Cmdliner = struct
  module Conv = struct
    open Rresult
    open Caml.Format
    let cfg =
      (fun str -> R.to_presult (Cfg.of_file str)),
      (fun ppf _cfg -> pp_print_string ppf "<cfg>")

    let hex =
      (fun str ->
         try `Ok (Hex.to_string (`Hex str))
         with _ ->`Error (sprintf "Hex expected, got %s" str)),
      (fun ppf hex ->
         let `Hex hex_str = Hex.of_string hex in
         fprintf ppf "%s" hex_str)

    let tezos_addr =
      (fun str ->
         match Base58.Tezos.of_string str with
         | Some addr -> `Ok addr
         | None -> `Error (sprintf "Tezos address expected, got %s" str)),
      Base58.Tezos.pp

    let payment_addr =
      (fun str ->
         match Base58.Bitcoin.of_string str with
         | Some addr -> `Ok addr
         | None -> `Error (sprintf "Bitcoin multisig address expected, got %s" str)),
      Base58.Bitcoin.pp

    let uri =
      (fun str -> try `Ok (Uri.of_string str) with _ -> `Error "Invalid uri"),
      Uri.pp_hum
  end

  open Cmdliner
  let cfg =
    let doc = "Configuration file to use." in
    Arg.(value & opt (some Conv.cfg) None & info ["c" ; "cfg"] ~doc)

  let loglevel =
    let doc = "Print more debug messages. Can be repeated." in
    Arg.(value & flag_all & info ["v"] ~doc)

  let json =
    let doc = "Output in JSON format." in
    Arg.(value & flag & info ["j" ; "json"] ~doc)
end

let getpass =
  let open Ctypes in
  Foreign.foreign "getpass" (string @-> returning string)

let getpass_confirm topic =
  let passwd = getpass ("Enter " ^ topic ^ ": ") in
  let passwd_confirm = getpass ("Confirm " ^ topic ^ ": ") in
  if String.compare passwd passwd_confirm <> 0 then None else Some (passwd)

let getpass topic = getpass ("Enter " ^ topic ^ ": ")

let getpass_confirm_clear topic =
  Stdio.printf "Enter %s: %!" topic ;
  let passwd = Stdio.In_channel.(input_line_exn stdin) in
  Stdio.printf "Confirm %s: %!" topic ;
  let passwd_confirm = Stdio.In_channel.(input_line_exn stdin) in
  if String.compare passwd passwd_confirm <> 0 then None else Some (passwd)

let getpass_clear topic =
  Stdio.printf "Enter %s: %!" topic ;
  Stdio.In_channel.(input_line_exn stdin)

module User = struct
  type t = {
    tezos_addr : Base58.Tezos.t ;
    scriptRedeem : Script.t ;
    payment_address : Base58.Bitcoin.t ;
  }

  let of_tezos_addr ~cfg tezos_addr =
    let scriptRedeem =
      Script.P2SH_multisig.scriptRedeem
        ~append_script:Script.Script.Opcode.[Data tezos_addr.Base58.Tezos.payload ; Drop]
        ~threshold:cfg.Cfg.threshold cfg.pks in
    let scriptRedeem = Script.of_script scriptRedeem in
    let payment_address =
      Payment_address.to_b58check
        (Payment_address.of_script scriptRedeem) in
    { tezos_addr ; scriptRedeem ; payment_address }
end

let pp_print_quoted_string ppf str =
  let open Caml.Format in
  fprintf ppf "\"%s\"" str

let pp_print_quoted_string_list ppf strs =
  let open Caml.Format in
  pp_print_list ~pp_sep:(fun ppf () -> pp_print_string ppf ", ")
    pp_print_quoted_string ppf strs
