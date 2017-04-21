open Sexplib.Std
open Libbitcoin

module Cfg = struct
  type t = {
    sks : string list ;
    pks : string list ;
    addrs : string list ;
    threshold : int ;
  } [@@deriving sexp]

  let default = {
    sks = [
      "cPUDdQmiqjMVcPWxn2zJxV2Kdm8G5fbkXyNh9Xd4qfhq6gVtbFCd" ;
      "cQ6aDw6R7fGExboo7MfrNDqXDNMQ8jyh8TTLK9DJmGnSPhvQ68rq" ;
      "cRh1K3z3LAcgVxHNA7aV6UYLKxWNEK4y5qPCJjHie2KPPQCmMBb2"
    ] ;
    pks = [
      "022f4a602f021cc2c7dd5536eeddff28173dc7f6bdd494e546487c0594942928bb" ;
      "03bf0ddb74ec76b17b0a55f4ad6a7844e5a75666ba2b2fc8c0fa84d2a382a83989" ;
      "03732373d6dfd7a03a5c3860c69790d8a7cb9538dc82593703af0bdd50c77f066c" ;
    ] ;
    addrs = [
      "mpoRpwTq46wqkXmBuMPP6GfPBLwcbziWci" ;
      "mmcaFxuYHekBukqbWCvVdvacWgTwtedCYu" ;
      "mmdANWHsdqp7XVQa8vXVRhemp9AP1NpC54" ;
    ] ;
    threshold = 2
  }
end

let set_loglevel vs =
  let level = match List.length vs with
    | 0 -> Lwt_log.Notice
    | 1 -> Info
    | _ -> Debug in
  Lwt_log.add_rule "*" level

module Cmdliner = struct
  open Cmdliner
  let cfg =
    let cfg_conv =
      let of_file fn =
        match Sexplib.Sexp.load_sexp_conv fn Cfg.t_of_sexp with
        | `Result cfg -> `Ok cfg
        | `Error _ -> `Error "cfg_of_file" in
      let pp ppf cfg =
        Sexplib.Sexp.pp_hum ppf (Cfg.sexp_of_t cfg) in
      of_file, pp in
    let doc = "Configuration file to use." in
    Arg.(value & opt cfg_conv Cfg.default & info ["c" ; "cfg"] ~doc)

  let loglevel =
    let doc = "Print more debug messages. Can be repeated." in
    Arg.(value & flag_all & info ["v"] ~doc)

  let testnet =
    let doc = "Use Bitcoin testnet." in
    Arg.(value & flag & info ["t" ; "testnet"] ~doc)

  module Conv = struct
    open Caml.Format
    let hex =
      (fun str ->
         try `Ok (Hex.to_string (`Hex str))
         with _ ->`Error (sprintf "Hex expected, got %s" str)),
      (fun ppf hex ->
         let `Hex hex_str = Hex.of_string hex in
         fprintf ppf "%s" hex_str)

    let payment_addr =
      (fun str ->
         match Payment_address.of_b58check str with
         | Some addr -> `Ok addr
         | None -> `Error (sprintf "Payment address expected, got %s" str)),
      Payment_address.pp
  end
end

let getpass =
  let open Ctypes in
  Foreign.foreign "getpass" (string @-> returning string)

let getpass_confirm () =
  let passwd = getpass "Enter passphrase: " in
  let passwd_confirm = getpass "Confirm passphrase: " in
  if passwd <> passwd_confirm then None else Some (passwd)

let getpass () = getpass "Enter passphrase: "
