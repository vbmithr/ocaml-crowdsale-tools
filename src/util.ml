open Base
open Libbitcoin

module Cfg = struct
  type t = {
    version : Ec_private.version ;
    sks : Ec_private.t list ;
    pks : Ec_public.t list ;
    addrs : Payment_address.t list ;
    addrs_testnet: Payment_address.t list ;
    threshold : int ;
  }

  let of_sks ?(version=Ec_private.Mainnet) sks =
    let sks = List.map ~f:Ec_private.of_wif_exn sks in
    let pks = List.map sks ~f:Ec_public.of_private in
    let addrs =
      List.map pks ~f:Payment_address.(of_point ~version:P2PKH) in
    let addrs_testnet =
      List.map pks ~f:Payment_address.(of_point ~version:Testnet_P2PKH) in
    { version ; sks ; pks ; addrs ; addrs_testnet ; threshold = 2 }

  let default = of_sks ~version:Testnet [
      "cPUDdQmiqjMVcPWxn2zJxV2Kdm8G5fbkXyNh9Xd4qfhq6gVtbFCd" ;
      "cQ6aDw6R7fGExboo7MfrNDqXDNMQ8jyh8TTLK9DJmGnSPhvQ68rq" ;
      "cRh1K3z3LAcgVxHNA7aV6UYLKxWNEK4y5qPCJjHie2KPPQCmMBb2"
    ]

  let version_encoding =
    Json_encoding.string_enum [
      "testnet", Ec_private.Testnet ;
      "mainnet", Mainnet ;
    ]

  let encoding =
    let open Json_encoding in
    conv
      (fun _ -> (Ec_private.Mainnet, [], [], 0))
      begin fun (version, sks, pks, threshold) ->
        if List.length sks > 0 then begin
          let sks = List.map sks ~f:(Ec_private.of_wif_exn ~version) in
          let pks = List.map sks ~f:Ec_public.of_private in
          let addrs =
            List.map pks ~f:Payment_address.(of_point ~version:P2PKH) in
          let addrs_testnet =
            List.map pks ~f:Payment_address.(of_point ~version:Testnet_P2PKH) in
          { version ; sks ; pks ; addrs ; addrs_testnet ; threshold }
        end
        else begin
          let pks = List.map pks ~f:(fun pk -> Ec_public.of_hex_exn (`Hex pk)) in
          let addrs =
            List.map pks ~f:Payment_address.(of_point ~version:P2PKH) in
          let addrs_testnet =
            List.map pks ~f:Payment_address.(of_point ~version:Testnet_P2PKH) in
          { version ; sks = [] ; pks ; addrs ; addrs_testnet ; threshold }
        end end
      (obj4
         (dft "version" version_encoding Ec_private.Mainnet)
         (dft "sks" (list string) [])
         (dft "pks" (list string) [])
         (req "threshold" int))

  let of_file fn =
    let json = Ezjsonm.from_channel (Stdio.In_channel.create fn) in
    Json_encoding.destruct encoding json
end

let set_loglevel vs =
  let level = match List.length vs with
    | 0 -> Lwt_log.Notice
    | 1 -> Info
    | _ -> Debug in
  Lwt_log.add_rule "*" level

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

    let payment_addr =
      (fun str ->
         match Base58.of_string str with
         | Some addr -> `Ok addr
         | None -> `Error (sprintf "Payment address expected, got %s" str)),
      Base58.pp
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
