open Base
open Libbitcoin

module Cfg = struct
  let keyPath = "44'/1'/0'/0/0"
  let threshold = 2
  let fees = 200
  let fees_testnet = 100

  type t = {
    testnet : bool ;
    keyPath : string ;
    sks : Ec_private.t list ;
    pks : Ec_public.t list ;
    addrs : Payment_address.t list ;
    addrs_testnet: Payment_address.t list ;
    threshold : int ;
    fees : int ;
    fees_testnet : int ;
  }

  let of_pks ?(testnet=false) ?(compressed=true) uncompressed_pks =
    let pk_of_bytes = match compressed with
      | true -> Ec_public.of_bytes_exn
      | false -> Ec_public.of_uncomp_point_exn in
    let sks = [] in
    let pks = List.map uncompressed_pks ~f:pk_of_bytes in
    let addrs =
      List.map pks ~f:Payment_address.(of_point ~version:P2PKH) in
    let addrs_testnet =
      List.map pks ~f:Payment_address.(of_point ~version:Testnet_P2PKH) in
    { testnet ; keyPath ; sks ; pks ; addrs ; addrs_testnet ;
      threshold ; fees ; fees_testnet }

  let of_sks ?(testnet=false) sks =
    let sks = List.map ~f:Ec_private.of_wif_exn sks in
    let pks = List.map sks ~f:Ec_public.of_private in
    let addrs =
      List.map pks ~f:Payment_address.(of_point ~version:P2PKH) in
    let addrs_testnet =
      List.map pks ~f:Payment_address.(of_point ~version:Testnet_P2PKH) in
    { testnet ; keyPath ; sks ; pks ; addrs ; addrs_testnet ;
      threshold ; fees ; fees_testnet }

  let default = of_sks ~testnet:true [
      "cPUDdQmiqjMVcPWxn2zJxV2Kdm8G5fbkXyNh9Xd4qfhq6gVtbFCd" ;
      "cQ6aDw6R7fGExboo7MfrNDqXDNMQ8jyh8TTLK9DJmGnSPhvQ68rq" ;
      "cRh1K3z3LAcgVxHNA7aV6UYLKxWNEK4y5qPCJjHie2KPPQCmMBb2"
    ]

  let encoding =
    let open Json_encoding in
    conv
      (fun { testnet ; keyPath ; sks ; pks ; addrs ; addrs_testnet ;
             threshold ; fees ; fees_testnet } ->
        (testnet, keyPath,
         List.map sks ~f:Ec_private.to_wif,
         List.map pks
           ~f:(fun pk -> let `Hex pk_hex = Ec_public.to_hex pk in pk_hex),
         threshold, fees, fees_testnet))
      begin fun (testnet, keyPath, sks, pks, threshold, fees, fees_testnet) ->
        if List.length sks > 0 then begin
          let sks = List.map sks ~f:(Ec_private.of_wif_exn ~testnet) in
          let pks = List.map sks ~f:Ec_public.of_private in
          let addrs =
            List.map pks ~f:Payment_address.(of_point ~version:P2PKH) in
          let addrs_testnet =
            List.map pks ~f:Payment_address.(of_point ~version:Testnet_P2PKH) in
          { testnet ; keyPath ; sks ; pks ; addrs ; addrs_testnet ; threshold ; fees ; fees_testnet }
        end
        else begin
          let pks = List.map pks ~f:(fun pk -> Ec_public.of_hex_exn (`Hex pk)) in
          let addrs =
            List.map pks ~f:Payment_address.(of_point ~version:P2PKH) in
          let addrs_testnet =
            List.map pks ~f:Payment_address.(of_point ~version:Testnet_P2PKH) in
          { testnet ; keyPath ; sks = [] ; pks ; addrs ; addrs_testnet ; threshold ; fees ; fees_testnet }
        end end
      (obj7
         (dft "testnet" bool false)
         (dft "keyPath" string keyPath)
         (dft "sks" (list string) [])
         (dft "pks" (list string) [])
         (req "threshold" int)
         (dft "fees" int fees)
         (dft "fees_testnet" int fees_testnet))

  let of_file fn =
    let json = Ezjsonm.from_channel (Stdio.In_channel.create fn) in
    Json_encoding.destruct encoding json

  let to_ezjsonm cfg =
    Json_encoding.construct encoding cfg

  let pp ppf cfg =
    Json_repr.(pp ~compact:false (module Ezjsonm) ppf (to_ezjsonm cfg))

  let to_string cfg =
    Caml.Format.asprintf "%a" pp cfg
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
    Arg.(value & opt Conv.cfg Cfg.default & info ["c" ; "cfg"] ~doc)

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
