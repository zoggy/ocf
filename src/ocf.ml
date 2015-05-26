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
let string_of_path = String.concat "."

type error =
| Json_error of string
| Invalid_value of Yojson.Safe.json
| Invalid_path of path
| Path_conflict of path
| Error_at_path of path * error
| Exn_at_path of path * exn

exception Error of error

let rec string_of_error = function
| Json_error msg ->
    Printf.sprintf "Error while reading JSON: %s" msg
| Invalid_value json ->
    Printf.sprintf "Invalid value %s" (Yojson.Safe.pretty_to_string json)
| Invalid_path p -> Printf.sprintf "Invalid path %S" (string_of_path p)
| Path_conflict p -> Printf.sprintf "Path conflict on %S" (string_of_path p)
| Error_at_path (p, e) ->
    Printf.sprintf "At %S: %s" (string_of_path p) (string_of_error e)
| Exn_at_path (p, e) ->
    Printf.sprintf "At %S: %s" (string_of_path p) (Printexc.to_string e)

let error e = raise (Error e)
let json_error s = error (Json_error s)
let invalid_value s = error (Invalid_value s)
let invalid_path p = error (Invalid_path p)
let path_conflict p = error (Path_conflict p)
let error_at_path p e = error (Error_at_path (p, e))
let exn_at_path p e = error (Exn_at_path (p, e))

module Wrapper =
  struct
    type 'a t = {
        to_json : 'a -> Yojson.Safe.json ;
        from_json : Yojson.Safe.json -> 'a ;
      }

    let wrapper to_json from_json = { to_json ; from_json }

    let int =
      let to_j n = `Int n in
      let from_j = function
        `Int n -> n
      | (`Intlit s)
      | (`String s) as json ->
          begin try int_of_string s with _ -> invalid_value json end
      | json -> invalid_value json
      in
      wrapper to_j from_j

    let float =
      let to_j x = `Float x in
      let from_j = function
        `Float x -> x
      | `Int n -> float n
      | (`Intlit s)
      | (`String s) as json ->
          begin try float_of_string s with _ -> invalid_value json end
      | json -> invalid_value json
      in
      wrapper to_j from_j

    let string_to_json x = `String x
    let string_from_json = function
        `String s -> s
      | json -> invalid_value json

    let string =
      wrapper string_to_json string_from_json

    let string_ to_str from_str =
      let to_j x = string_to_json (to_str x) in
      let from_j x = from_str (string_from_json x) in
      wrapper to_j from_j

    let list w =
      let to_j l = `List (List.map w.to_json l) in
      let from_j = function
      | `List l
      | `Tuple l -> List.map w.from_json l
      | `Null -> []
      | json -> invalid_value json
      in
      wrapper to_j from_j

    let option w =
      let to_j = function None -> `Null | Some x -> w.to_json x in
      let from_j = function
        `Null -> None
      | x -> Some (w.from_json x)
      in
      wrapper to_j from_j

    let pair w1 w2 =
      let to_j (v1, v2) = `Tuple [w1.to_json v1 ; w2.to_json v2] in
      let from_j = function
        `List [v1 ; v2]
      | `Tuple [v1 ; v2] -> (w1.from_json v1, w2.from_json v2)
      | json -> invalid_value json
      in
      wrapper to_j from_j

    let triple w1 w2 w3 =
      let to_j (v1, v2, v3) =
        `Tuple [w1.to_json v1 ; w2.to_json v2 ; w3.to_json v3]
      in
      let from_j = function
        `List [v1 ; v2 ; v3]
      | `Tuple [v1 ; v2 ; v3] ->
          (w1.from_json v1, w2.from_json v2, w3.from_json v3)
      | json -> invalid_value json
      in
      wrapper to_j from_j
  end

type 'a wrapper = 'a Wrapper.t
type conf_option_ =
  { wrapper : 'a. 'a wrapper ;
    mutable value : 'a. 'a ;
    desc : string option ;
    cb : 'a. ('a -> unit) option ;
  }

type 'a conf_option = conf_option_

let get o = o.value

let option : ?desc: string -> ?cb: ('a -> unit) ->
  'a wrapper -> 'a -> 'a conf_option =
  fun ?desc ?cb wrapper value ->
    { wrapper = Obj.magic wrapper ;
      value = Obj.magic value ;
      desc ;
      cb = Obj.magic cb ;
    }

let int ?desc ?cb n = option ?desc ?cb Wrapper.int n
let float ?desc ?cb x = option ?desc ?cb Wrapper.float x
let string ?desc ?cb s = option ?desc ?cb Wrapper.string s
let list ?desc ?cb w l = option ?desc ?cb (Wrapper.list w) l
let option_ ?desc ?cb w l = option ?desc ?cb (Wrapper.option w) l
let pair ?desc ?cb w1 w2 x = option ?desc ?cb (Wrapper.pair w1 w2) x
let triple ?desc ?cb w1 w2 w3 x = option ?desc ?cb (Wrapper.triple w1 w2 w3) x

type node =
  | Option of conf_option_
  | Section of group

and group = node SMap.t

let group = SMap.empty

let rec add ?(acc_path=[]) group path node =
  match path with
    [] -> invalid_path []
  | [h] ->
      begin
        match SMap.find h group with
        | exception Not_found ->
            SMap.add h node group
        | _ ->
            path_conflict (List.rev (h::acc_path))
      end
  | h :: q ->
      match SMap.find h group with
      | exception Not_found ->
          let map = add
            ~acc_path: (h::acc_path) SMap.empty q node
          in
          SMap.add h (Section map) group
      | Option _ ->
          path_conflict (List.rev (h::acc_path))
      | Section _ when q = [] ->
          path_conflict (List.rev (h::acc_path))
      | Section map ->
          let map = add
            ~acc_path: (h::acc_path) map q node
          in
          SMap.add h (Section map) group

let add_group group path g = add ?acc_path: None group path (Section g)
let add group path option = add ?acc_path: None group path (Option option)

let from_json_option path option json =
  try
    option.value <- option.wrapper.Wrapper.from_json json ;
    match option.cb with
      None -> ()
    | Some f -> f option.value
  with
    Error e -> error_at_path path e
  | e -> exn_at_path path e

let rec from_json_group =
  let f path assocs str node =
    match SMap.find str assocs with
    | exception Not_found -> ()
    | json ->
        match node with
          Option o -> from_json_option (List.rev (str :: path)) o json
        | Section map ->
            from_json_group ~path: (str :: path) map json
  in
  fun ?(path=[]) map json ->
    match json with
      `Assoc assocs ->
        let assocs = List.fold_left
          (fun acc (k,v) -> SMap.add k v acc) SMap.empty assocs
        in
        SMap.iter (f path assocs) map
    | _ -> invalid_value json

let from_json = from_json_group ?path: None

let from_string map str =
  try
    let json = Yojson.Safe.from_string str in
    from_json map json
  with Yojson.Json_error msg ->
    json_error msg

let from_file map file =
  try
    let json = Yojson.Safe.from_file file in
    from_json map json
  with
    Yojson.Json_error msg ->
      json_error msg

let to_json_option option = option.wrapper.Wrapper.to_json option.value

let rec to_json_group map =
  let f name node acc =
    match node with
    | Section map -> (name, to_json_group map) :: acc
    | Option o ->
        let acc = (name, to_json_option o) :: acc in
        match o.desc with
          None -> acc
        | Some str -> (name, `String str) :: acc
  in
  `Assoc (SMap.fold f map [])

let to_json = to_json_group

let to_string map = Yojson.Safe.pretty_to_string (to_json map)
let to_file map file =
  let oc = open_out file in
  Yojson.Safe.pretty_to_channel oc (to_json map);
  close_out oc

