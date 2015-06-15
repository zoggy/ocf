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

(** Reading and writing configuration files in JSON syntax *)

(** To represent option names from root. *)
type path = string list

module SMap : Map.S with type key = string

(** {2 Errors} *)

type error =
| Json_error of string (** JSON could not be read or written *)
| Invalid_value of Yojson.Safe.json (** Unexpected JSON value *)
| Invalid_path of path  (** Invalid path used (empty list) *)
| Path_conflict of path (** When adding an option, an option path
                            cannot be the prefix of another one. *)
| Error_at_path of path * error
   (** Error while reading the option at the given path. *)
| Exn_at_path of path * exn
   (** Exception raised while reading the option at the given path *)

exception Error of error

val string_of_error : error -> string

(** {3 Convenient functions to raise [Error]} *)

val json_error : string -> 'a
val invalid_value : Yojson.Safe.json -> 'a
val invalid_path : path -> 'a
val path_conflict : path -> 'a
val error_at_path : path -> error -> 'a
val exn_at_path : path -> exn -> 'a

(** {2 Wrappers}

A wrapper is a pair of functions to read and write values of
some type from and to JSON.
*)

module Wrapper : sig
    type 'a t = {
        to_json : 'a -> Yojson.Safe.json ;
        from_json : ?def: 'a -> Yojson.Safe.json -> 'a ;
      }

    val make : ('a -> Yojson.Safe.json) ->
      (?def: 'a -> Yojson.Safe.json -> 'a) -> 'a t
    val int : int t
    val float : float t
    val string : string t
    val string_ : ('a -> string) -> (string -> 'a) -> 'a t
    val list : 'a t -> 'a list t
    val option : 'a t -> 'a option t
    val pair : 'a t -> 'b t -> ('a * 'b) t
    val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
  end

type 'a wrapper = 'a Wrapper.t

(** {2 Options and option groups} *)

(** An option with a value of type ['a]. When the option is found
  in a JSON code (see {!input}), the value is modified in place.
  Use {!get} to retrieve the option value. *)
type 'a conf_option

(** [option wrapper v] creates an option with initial value [v]
  and using the given [wrapper] to read and write from and to JSON.
  @param doc an optional description for the option.
  @param cb an optional function to call each time the option is read.
*)
val option : ?doc: string -> ?cb: ('a -> unit) ->
  'a wrapper -> 'a -> 'a conf_option

(** [get option] returns the value of the given option. *)
val get : 'a conf_option -> 'a

(** [set option value] sets the value of the given option
  and calls the associated callback if any. *)
val set : 'a conf_option -> 'a -> unit

(** A group is used to group options and other groups. *)
type group

(** Create a new empty group. *)
val group : group

(** [add group path option] adds the given [option] to
  [group] at [path]. *)
val add : group -> path -> 'a conf_option -> group

(** [add_group group path g] adds the group [g] to [group] at [path].*)
val add_group : group -> path -> group -> group

(** {3:convenient Convenient functions to create options} *)

val int : ?doc: string -> ?cb: (int -> unit) -> int -> int conf_option
val float : ?doc: string -> ?cb: (float -> unit) -> float -> float conf_option
val string : ?doc: string -> ?cb: (string -> unit) -> string -> string conf_option
val list : ?doc: string -> ?cb: ('a list -> unit) -> 'a wrapper ->
  'a list -> 'a list conf_option
val option_ : ?doc: string -> ?cb: ('a option -> unit) ->
  'a wrapper -> 'a option -> 'a option conf_option
val pair : ?doc: string -> ?cb: ('a * 'b -> unit) ->
  'a wrapper -> 'b wrapper -> 'a * 'b -> ('a * 'b) conf_option
val triple : ?doc: string -> ?cb: ('a * 'b * 'c -> unit) ->
  'a wrapper -> 'b wrapper -> 'c wrapper ->
    'a * 'b * 'c -> ('a * 'b * 'c) conf_option

(** {2:input Reading options} *)

val from_json : group -> Yojson.Safe.json -> unit
val from_string : group -> string -> unit
val from_file : group -> string -> unit


(** {2:output Writing options}

The following functions output the current state of the group, i.e.
the options it contains, with their current value.

The [with_doc] parameter indicates whether to output doc associated
to options. Default is [true].
*)

val to_json : ?with_doc: bool -> group -> Yojson.Safe.json
val to_string : ?with_doc: bool -> group -> string
val to_file : ?with_doc: bool -> group -> string -> unit

(** {2 Building command line arguments} *)

(** [to_arg option key] build a command line option description
  to use with the [Arg] module. If [doc] is not provided, then
  the description string of the option is used, if there is one;
  else the doc string is empty (preventing the option to appear
  in the list of options, see [Arg] module documentation).
  [key] is the option name, like ["-n"].
*)
val to_arg :
  'a conf_option -> ?doc: Arg.doc -> Arg.key -> Arg.key * Arg.spec * Arg.doc

