type coin_type =
  | Bitcoin
  | Bitcoin_testnet

let int_of_coin_type = function
  | Bitcoin -> 0
  | Bitcoin_testnet -> 1

let coin_type_of_int = function
  | 0 -> Bitcoin
  | 1 -> Bitcoin_testnet
  | _ -> invalid_arg "coin_type_of_int"

let pp_coin_type ppf ct =
  Format.pp_print_int ppf (int_of_coin_type ct)

type chain =
  | External
  | Internal

let int_of_chain = function
  | External -> 0
  | Internal -> 1

let chain_of_int = function
  | 0 -> External
  | 1 -> Internal
  | _ -> invalid_arg "chain_of_int"

let pp_chain ppf chain =
  Format.pp_print_int ppf (int_of_chain chain)

type purpose = BIP44

let int_of_purpose = function
  | BIP44 -> 44

let purpose_of_int = function
  | 44 -> BIP44
  | _ -> invalid_arg "purpose_of_int"

let pp_purpose ppf purpose =
  Format.pp_print_int ppf (int_of_purpose purpose)

type t = {
  purpose : purpose ;
  coin_type : coin_type ;
  account : int ;
  chain : chain ;
  index : int ;
}

let create
    ?(purpose=BIP44) ?(coin_type=Bitcoin)
    ?(account=0) ?(chain=External) ?(index=0) () =
  { purpose ; coin_type ; account ; chain ; index }

let pp ppf { purpose ; coin_type ; account ; chain ; index } =
  Format.fprintf ppf "%a'/%a'/%d'/%a/%d"
    pp_purpose purpose pp_coin_type
    coin_type account pp_chain chain index

let to_string t =
  Format.asprintf "%a" pp t

let of_string str =
  try
    Scanf.sscanf str "%d'/%d'/%d'/%d/%d"
      (fun purpose cointype account chain index ->
         let purpose = purpose_of_int purpose in
         let coin_type = coin_type_of_int cointype in
         let chain = chain_of_int chain in
         Some { purpose ; coin_type ; account ; chain ; index })
  with _ -> None

let of_string_exn str =
  match of_string str with
  | None -> invalid_arg "Bip44.of_string_exn"
  | Some t -> t
