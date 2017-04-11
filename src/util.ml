open Sexplib.Std

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

let loglevel_of_int = function 2 -> `Info | 3 -> `Debug | _ -> `Error
