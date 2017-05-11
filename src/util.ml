open Base
open Libbitcoin

module Cfg = struct
  let default_location =
    Caml.Filename.concat (Unix.getenv "HOME") ".tezos-crowdsale"
  let threshold = 2
  let fees = 200

  type t = {
    testnet : bool ;
    sks : Ec_private.t list ;
    pks : Ec_public.t list ;
    addrs : Payment_address.t list ;
    threshold : int ;
    fees : int ;
  }

  let of_pks ?(testnet=false) pks =
    let version = Base58.Bitcoin.(if testnet then Testnet_P2PKH else P2PKH) in
    let sks = [] in
    let pks = List.map pks ~f:Ec_public.of_bytes_exn in
    let addrs =
      List.map pks ~f:(Payment_address.of_point ~version) in
    { testnet ; sks ; pks ; addrs ;
      threshold ; fees }

  let of_sks ?(testnet=false) sks =
    let version = Base58.Bitcoin.(if testnet then Testnet_P2PKH else P2PKH) in
    let sks = List.map ~f:Ec_private.of_wif_exn sks in
    let pks = List.map sks ~f:Ec_public.of_private in
    let addrs =
      List.map pks ~f:(Payment_address.of_point ~version) in
    { testnet ; sks ; pks ; addrs ;
      threshold ; fees }

  let encoding =
    let open Json_encoding in
    conv
      begin fun { testnet ; sks ; pks ; addrs ; threshold ; fees } ->
        let sks = List.map sks ~f:Ec_private.to_wif in
        let pks = List.map pks
            ~f:(fun pk -> let `Hex pk_hex = Ec_public.to_hex pk in pk_hex) in
        (testnet, sks, pks, threshold, fees)
      end
      begin fun (testnet, sks, pks, threshold, fees) ->
        let version = Base58.Bitcoin.(if testnet then Testnet_P2PKH else P2PKH) in
        if List.length sks > 0 then begin
          let sks = List.map sks ~f:(Ec_private.of_wif_exn ~testnet) in
          let pks = List.map sks ~f:Ec_public.of_private in
          let addrs =
            List.map pks ~f:(Payment_address.of_point ~version) in
          { testnet ; sks ; pks ; addrs ; threshold ; fees }
        end
        else begin
          let pks = List.map pks ~f:(fun pk -> Ec_public.of_hex_exn (`Hex pk)) in
          let addrs =
            List.map pks ~f:(Payment_address.of_point ~version) in
          { testnet ; sks = [] ; pks ; addrs ; threshold ; fees }
        end
      end
      (obj5
         (dft "testnet" bool false)
         (dft "sks" (list string) [])
         (dft "pks" (list string) [])
         (dft "threshold" int threshold)
         (dft "fees" int fees))

  let of_file fn =
    let json = Ezjsonm.from_channel (Stdio.In_channel.create fn) in
    Json_encoding.destruct encoding json

  let to_ezjsonm cfg =
    Json_encoding.construct encoding cfg

  let pp ppf cfg =
    Json_repr.(pp ~compact:false (module Ezjsonm) ppf (to_ezjsonm cfg))

  let to_string cfg =
    Caml.Format.asprintf "%a" pp cfg

  let unopt = function
    | None -> of_file default_location
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
    open Caml.Format
    let cfg =
      (fun str -> try `Ok (Cfg.of_file str) with _ -> `Error "Cfg.of_file"),
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
  end

  open Cmdliner
  let cfg =
    let doc = "Configuration file to use." in
    Arg.(value & opt (some Conv.cfg) None & info ["c" ; "cfg"] ~doc)

  let loglevel =
    let doc = "Print more debug messages. Can be repeated." in
    Arg.(value & flag_all & info ["v"] ~doc)

  let testnet =
    let doc = "Use Bitcoin testnet." in
    Arg.(value & flag & info ["t" ; "testnet"] ~doc)

  let json =
    let doc = "Output in JSON format." in
    Arg.(value & flag & info ["j" ; "json"] ~doc)
end

let getpass =
  let open Ctypes in
  Foreign.foreign "getpass" (string @-> returning string)

let getpass_confirm () =
  let passwd = getpass "Enter passphrase: " in
  let passwd_confirm = getpass "Confirm passphrase: " in
  if String.compare passwd passwd_confirm <> 0 then None else Some (passwd)

let getpass () = getpass "Enter passphrase: "
