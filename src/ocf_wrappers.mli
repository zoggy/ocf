(** Wrappers *)

open Ocf_types

val int : int wrapper
val float : float wrapper
val string : string wrapper
val list : 'a wrapper -> 'a list wrapper
val option : 'a wrapper -> 'a option wrapper
val pair : 'a wrapper -> 'b wrapper -> ('a * 'b) wrapper
val triple :
  'a wrapper -> 'b wrapper -> 'c wrapper -> ('a * 'b * 'c) wrapper



