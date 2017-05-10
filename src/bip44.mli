type purpose = BIP44

type coin_type =
  | Bitcoin
  | Bitcoin_testnet

type chain =
  | External
  | Internal

type t = {
  purpose : purpose ;
  coin_type : coin_type ;
  account : int ;
  chain : chain ;
  index : int ;
}

val create :
  ?purpose:purpose -> ?coin_type:coin_type ->
  ?account:int -> ?chain:chain -> ?index:int ->
  unit -> t

val of_string : string -> t option
val of_string_exn : string -> t

val pp : Format.formatter -> t -> unit
val to_string : t -> string
