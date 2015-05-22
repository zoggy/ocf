(*********************************************************************************)
(*                Ocf                                                            *)
(*                                                                               *)
(*    Copyright (C) 2015 INRIA. All rights reserved.                             *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU Lesser General Public License as             *)
(*    published by the Free Software Foundation, version 3 of the License.       *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *)
(*    GNU Library General Public License for more details.                       *)
(*                                                                               *)
(*    You should have received a copy of the GNU Lesser General Public           *)
(*    License along with this program; if not, write to the Free Software        *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    As a special exception, you have permission to link this program           *)
(*    with the OCaml compiler and distribute executables, as long as you         *)
(*    follow the requirements of the GNU GPL in regard to all of the             *)
(*    software in the executable aside from the OCaml compiler.                  *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*                                                                               *)
(*********************************************************************************)


type path = string list

type error =
| Json_error of string
| Invalid_value of Yojson.Safe.json
| Invalid_path of path
| Path_conflict of path
| Error_at_path of path * error
| Exn_at_path of path * exn

exception Error of error

val string_of_error : error -> string

val json_error : string -> 'a
val invalid_value : Yojson.Safe.json -> 'a
val invalid_path : path -> 'a
val path_conflict : path -> 'a
val error_at_path : path -> error -> 'a
val exn_at_path : path -> exn -> 'a

module Wrapper : sig
    type 'a t = {
        to_json : 'a -> Yojson.Safe.json ;
        from_json : Yojson.Safe.json -> 'a ;
      }

    val int : int t
    val float : float t
    val string : string t
    val list : 'a t -> 'a list t
    val option : 'a t -> 'a option t
    val pair : 'a t -> 'b t -> ('a * 'b) t
    val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
  end

type 'a wrapper = 'a Wrapper.t
type 'a conf_option
type group

val option : ?desc: string -> ?cb: ('a -> unit) ->
  'a wrapper -> 'a -> 'a conf_option

val int : ?desc: string -> ?cb: (int -> unit) -> int -> int conf_option
val float : ?desc: string -> ?cb: (float -> unit) -> float -> float conf_option
val string : ?desc: string -> ?cb: (string -> unit) -> string -> string conf_option
val list : ?desc: string -> ?cb: ('a list -> unit) -> 'a wrapper -> 
  'a list -> 'a list conf_option
val option_ : ?desc: string -> ?cb: ('a option -> unit) ->
  'a wrapper -> 'a option -> 'a option conf_option
val pair : ?desc: string -> ?cb: ('a * 'b -> unit) ->
  'a wrapper -> 'b wrapper -> 'a * 'b -> ('a * 'b) conf_option
val triple : ?desc: string -> ?cb: ('a * 'b * 'c -> unit) ->
  'a wrapper -> 'b wrapper -> 'c wrapper ->
    'a * 'b * 'c -> ('a * 'b * 'c) conf_option

val get : 'a conf_option -> 'a

val group : group
val add : group -> path -> 'a conf_option -> group
val add_group : group -> path -> group -> group

val from_json : group -> Yojson.Safe.json -> unit
val from_string : group -> string -> unit
val from_file : group -> string -> unit

val to_json : group -> Yojson.Safe.json
val to_string : group -> string
val to_file : group -> string -> unit
