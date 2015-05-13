(** Wrappers *)

exception Invalid_value of string

type 'a wrapper = {
    to_json : 'a -> Yojson.Safe.json ;
    from_json : Yojson.Safe.json -> 'a ;
  }

val int : int wrapper
val float : float wrapper
val string : string wrapper
val list : 'a wrapper -> 'a list wrapper
val pair : 'a wrapper -> 'b wrapper -> ('a * 'b) wrapper
val triple :
  'a wrapper -> 'b wrapper -> 'c wrapper -> ('a * 'b * 'c) wrapper



