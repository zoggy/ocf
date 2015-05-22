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

module SMap = Map.Make(String)

type path = string list

type error =
| Invalid_value of Yojson.Safe.json
| Invalid_path of path
| Path_conflict of path

exception Error of error

let string_of_error = function
| Invalid_value json ->
    Printf.sprintf "Invalid value %s" (Yojson.Safe.to_string json)
| Invalid_path p -> Printf.sprintf "Invalid path %S" (String.concat "." p)
| Path_conflict p -> Printf.sprintf "Path conflict on %S" (String.concat "." p)

let error e = raise (Error e)
let invalid_value s = error (Invalid_value s)
let invalid_path p = error (Invalid_path p)
let path_conflict p = error (Path_conflict p)

type 'a wrapper = {
    to_json : 'a -> Yojson.Safe.json ;
    from_json : Yojson.Safe.json -> 'a ;
  }

type conf_option_ =
  { wrappers : 'a. 'a wrapper ;
    value : 'a. 'a ;
    desc : string option ;
  }

type 'a conf_option = conf_option_

let option : ?desc: string -> 'a wrapper -> 'a -> 'a conf_option =
  fun ?desc wrappers value ->
    { wrappers = Obj.magic wrappers ;
      value = Obj.magic value ;
      desc ;
    }

type node =
  | Option of conf_option_
  | Section of group

and group = node SMap.t

let group = SMap.empty

let rec add_option ?(acc_path=[]) group path option =
  match path with
    [] -> invalid_path []
  | h :: q ->
      match SMap.find h group with
      | exception Not_found ->
          let map = add_option
            ~acc_path: (h::acc_path) SMap.empty q option
          in
          SMap.add h (Section map) group
      | Option _ ->
          path_conflict (List.rev (h::acc_path))
      | Section _ when q = [] ->
          path_conflict (List.rev (h::acc_path))
      | Section map ->
          let map = add_option
            ~acc_path: (h::acc_path) map q option
          in
          SMap.add h (Section map) group





