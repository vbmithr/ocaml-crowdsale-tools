open Base
open Secp256k1
open Bitcoin.Wallet

let ctx = Context.create []
let pk1 = Key.read_pk_exn ctx (Hex.to_cstruct (`Hex "04e8f164c9c12b039d3e522aa291e3544ddc99240a878080669aee1c359f66d253793af56acd2af002bf7038570913ed5092328ef2940ce3cedaa7c1801174e50e")).buffer
let pk2 = Key.read_pk_exn ctx (Hex.to_cstruct (`Hex "04c6ad7f526553affb3979483dac278b1517199a5ac3cd0fbb669644aadbf933bafb53bc3ef4687b32399e8fae8444800231bf84cd41442c24abfc58c05ba77a75")).buffer
let fees_per_byte = 400
let vendor_id = 0x2C97
let product_id = 0x0001

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
    let hex =
      (fun str ->
         try `Ok (Hex.to_string (`Hex str))
         with _ ->`Error (sprintf "Hex expected, got %s" str)),
      (fun ppf hex ->
         let `Hex hex_str = Hex.of_string hex in
         fprintf ppf "%s" hex_str)

    let hex_cstruct =
      (fun str ->
         try `Ok (Hex.to_cstruct (`Hex str))
         with _ ->`Error (sprintf "Hex expected, got %s" str)),
      (fun ppf hex ->
         let `Hex hex_str = Hex.of_cstruct hex in
         fprintf ppf "%s" hex_str)

    let tezos_addr c =
      (fun str ->
         match Base58.Tezos.of_string c str with
         | Some addr -> `Ok addr
         | None -> `Error (sprintf "Tezos address expected, got %s" str)),
      (Base58.Tezos.pp c)

    let payment_addr c =
      (fun str ->
         match Base58.Bitcoin.of_string c str with
         | Some addr -> `Ok addr
         | None -> `Error (sprintf "Bitcoin multisig address expected, got %s" str)),
      Base58.Bitcoin.pp

    let path =
      (fun str ->
         match Bitcoin.Wallet.KeyPath.of_string str with
         | Some path -> `Ok path
         | None -> `Error (sprintf "Key path expected, got %s" str)),
      Bitcoin.Wallet.KeyPath.pp

    let network =
      let open Blockexplorer_lwt in
      (fun str ->
         try `Ok (network_of_string str) with _ ->
           `Error (Printf.sprintf "Network expected, got %s" str)),
      network_pp
  end

  open Cmdliner
  let loglevel =
    let doc = "Print more debug messages. Can be repeated." in
    Arg.(value & flag_all & info ["v"] ~doc)

  let testnet =
    let doc = "Use Bitcoin testnet." in
    Arg.(value & flag & info ["t" ; "testnet"] ~doc)

  let network =
    let doc = "Network to use." in
    Arg.(value & opt Conv.network Blockexplorer_lwt.BTC & info ["n" ; "network"] ~doc)

  let json =
    let doc = "Output in JSON format." in
    Arg.(value & flag & info ["j" ; "json"] ~doc)
end

let multisig_script tezos_pkh signers_pks =
  let open Bitcoin in
  let start_script =
    Script.Element.[O (Op_pushdata (Cstruct.len tezos_pkh)) ; D tezos_pkh ; O Op_drop] in
  let signers_pkh = List.map signers_pks ~f:begin fun pk ->
      let cs = Cstruct.of_bigarray (Key.to_bytes ctx pk) in
      Script.Element.[O (Op_pushdata (Cstruct.len cs)) ; D cs]
    end in
  let end_script = Script.Element.[O Op_2 ; O Op_checkmultisig] in
  let signers_pkh = Script.Element.O Op_2 :: (List.concat signers_pkh) in
  start_script @ signers_pkh @ end_script

let getpass () =
  let open Unix in
  (* Turn echoing off and fail if we can't. *)
  let tio = tcgetattr stdin in
  let old_echo = tio.c_echo in
  let old_echonl = tio.c_echonl in
  tio.c_echo <- false ;
  tio.c_echonl <- true ;
  tcsetattr stdin TCSAFLUSH tio ;
  (* Read the passwd. *)
  let passwd = Stdio.In_channel.(input_line_exn stdin) in
  (* Restore terminal. *)
  tio.c_echo <- old_echo ;
  tio.c_echonl <- old_echonl ;
  tcsetattr stdin TCSAFLUSH tio ;
  passwd

let getpass_confirm () =
  Stdio.printf "Input passphrase: %!" ;
  let pw = getpass () in
  Stdio.printf "Confirm passphrase: %!" ;
  let pw2 = getpass () in
  if String.equal pw pw2 then Some pw else None
