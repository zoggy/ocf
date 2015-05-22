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
| At_path of path * error

exception Error of error

val string_of_error : error -> string

val json_error : string -> 'a
val invalid_value : Yojson.Safe.json -> 'a
val invalid_path : path -> 'a
val path_conflict : path -> 'a
val error_at_path : path -> error -> 'a

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

val option : ?desc: string -> 'a wrapper -> 'a -> 'a conf_option
val get : 'a conf_option -> 'a
val add : group -> path -> 'a conf_option -> group

val from_json : group -> Yojson.Safe.json -> unit
val from_string : group -> string -> unit
val from_file : group -> string -> unit

val to_json : group -> Yojson.Safe.json
val to_string : group -> string
val to_file : group -> string -> unit
