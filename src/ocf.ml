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
    Printf.sprintf "%S: %s" (string_of_path p) (string_of_error e)
| Exn_at_path (p, e) ->
    Printf.sprintf "%S: %s" (string_of_path p) (Printexc.to_string e)

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
        to_json : ?with_doc: bool -> 'a -> Yojson.Safe.json ;
        from_json : ?def: 'a -> Yojson.Safe.json -> 'a ;
      }

    let make to_json from_json = { to_json ; from_json }
    let of_ok_error f json =
      match f json with
        `Ok x -> x
      | `Error msg -> invalid_value json

    let int =
      let to_j ?with_doc n = `Int n in
      let from_j ?def = function
        `Int n -> n
      | (`Intlit s)
      | (`String s) as json ->
          begin try int_of_string s with _ -> invalid_value json end
      | json -> invalid_value json
      in
      make to_j from_j

    let float =
      let to_j ?with_doc x = `Float x in
      let from_j ?def = function
        `Float x -> x
      | `Int n -> float n
      | (`Intlit s)
      | (`String s) as json ->
          begin try float_of_string s with _ -> invalid_value json end
      | json -> invalid_value json
      in
      make to_j from_j

    let bool =
      let to_j ?with_doc x = `Bool x in
      let from_j ?def = function
        `Bool x -> x
      | json -> invalid_value json
      in
      make to_j from_j

    let string_to_json ?with_doc x = `String x
    let string_from_json ?def = function
    | `Intlit s
    | `String s -> s
    | `Int n -> string_of_int n
    | json -> invalid_value json

    let string =
      make string_to_json string_from_json

    let string_ to_str from_str =
      let to_j ?with_doc x = string_to_json ?with_doc (to_str x) in
      let from_j ?def x = from_str (string_from_json x) in
      make to_j from_j

    let list w =
      let to_j ?with_doc l = `List (List.map (w.to_json ?with_doc) l) in
      let from_j ?def = function
      | `List l
      | `Tuple l -> List.map (w.from_json ?def: None) l
      | `Null -> []
      | json -> invalid_value json
      in
      make to_j from_j

    let option w =
      let to_j ?with_doc = function
      | None -> `Null
      | Some x -> w.to_json ?with_doc x
      in
      let from_j ?def = function
        `Null -> None
      | x -> Some (w.from_json x)
      in
      make to_j from_j

    let pair w1 w2 =
      let to_j ?with_doc (v1, v2) =
        `Tuple [w1.to_json ?with_doc v1 ; w2.to_json ?with_doc v2]
      in
      let from_j ?def = function
        `List [v1 ; v2]
      | `Tuple [v1 ; v2] -> (w1.from_json v1, w2.from_json v2)
      | json -> invalid_value json
      in
      make to_j from_j

    let triple w1 w2 w3 =
      let to_j ?with_doc (v1, v2, v3) =
        `Tuple [
          w1.to_json ?with_doc v1 ;
          w2.to_json ?with_doc v2 ;
          w3.to_json ?with_doc v3 ;
        ]
      in
      let from_j ?def = function
        `List [v1 ; v2 ; v3]
      | `Tuple [v1 ; v2 ; v3] ->
          (w1.from_json v1, w2.from_json v2, w3.from_json v3)
      | json -> invalid_value json
      in
      make to_j from_j

    type assocs = (string * Yojson.Safe.json) list
    let string_map ~fold ~add ~empty w =
      let to_j ?with_doc map =
        let l =
          fold
            (fun k v acc -> (k, w.to_json ?with_doc v) :: acc)
            map []
        in
        `Assoc l
      in
      let from_j ?def = function
      | `Assoc l ->
          List.fold_left
            (fun map (k, v) ->
               add k (w.from_json v) map)
            empty l
      | json -> invalid_value json
      in
      make to_j from_j
  end

type 'a wrapper = 'a Wrapper.t
type conf_option_ =
  { wrapper : 'a. 'a wrapper ;
    mutable value : 'a. 'a ;
    doc : string option ;
    cb : 'a. ('a -> unit) option ;
  }

type 'a conf_option = conf_option_

let get o = o.value
let set (o : 'a conf_option) (v : 'a) =
  o.value <- Obj.magic v;
  match o.cb with
  | None -> ()
  | Some f -> f v

let option : ?doc: string -> ?cb: ('a -> unit) ->
  'a wrapper -> 'a -> 'a conf_option =
  fun ?doc ?cb wrapper value ->
    { wrapper = Obj.magic wrapper ;
      value = Obj.magic value ;
      doc ;
      cb = Obj.magic cb ;
    }

let int ?doc ?cb n = option ?doc ?cb Wrapper.int n
let float ?doc ?cb x = option ?doc ?cb Wrapper.float x
let bool ?doc ?cb x = option ?doc ?cb Wrapper.bool x
let string ?doc ?cb s = option ?doc ?cb Wrapper.string s
let list ?doc ?cb w l = option ?doc ?cb (Wrapper.list w) l
let option_ ?doc ?cb w l = option ?doc ?cb (Wrapper.option w) l
let pair ?doc ?cb w1 w2 x = option ?doc ?cb (Wrapper.pair w1 w2) x
let triple ?doc ?cb w1 w2 w3 x = option ?doc ?cb (Wrapper.triple w1 w2 w3) x
let string_map ?doc ?cb ~fold ~add ~empty w x =
  option ?doc ?cb (Wrapper.string_map ~fold ~add ~empty w) x

type node =
  | Option of conf_option_
  | Group of node SMap.t

and 'a group = node
let group = Group SMap.empty

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
          SMap.add h (Group map) group
      | Option _ ->
          path_conflict (List.rev (h::acc_path))
      | Group _ when q = [] ->
          path_conflict (List.rev (h::acc_path))
      | Group map ->
          let map = add
            ~acc_path: (h::acc_path) map q node
          in
          SMap.add h (Group map) group

let add_group group path g =
  match group with
    Option _ -> assert false
  | Group map -> Group (add ?acc_path: None map path g)

let add group path option =
  match group with
  | Option _ -> assert false
  | Group map -> Group (add ?acc_path: None map path (Option option))

let as_group o = Option o

let from_json_option path option json =
  try
    let v = option.wrapper.Wrapper.from_json
      ~def: option.value json
    in
    set option v
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
        | Group map ->
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

let from_json = function
  Option o -> from_json_option [] o
| Group g -> from_json_group ?path: None g

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

let to_json_option ?with_doc option =
  option.wrapper.Wrapper.to_json ?with_doc option.value

let rec to_json_group ?with_doc map =
  let f name node acc =
    match node with
    | Group map -> (name, to_json_group ?with_doc map) :: acc
    | Option o ->
        let acc = (name, to_json_option ?with_doc o) :: acc in
        match with_doc, o.doc with
        | Some true, Some str -> (name, `String str) :: acc
        | _, _ -> acc
  in
  `Assoc (SMap.fold f map [])

let to_json ?(with_doc=true) = function
| Option o -> to_json_option ~with_doc o
| Group g -> to_json_group ~with_doc g

let to_string ?with_doc map =
  Yojson.Safe.pretty_to_string (to_json ?with_doc map)

let to_file ?with_doc map file =
  let oc = open_out file in
  Yojson.Safe.pretty_to_channel oc (to_json ?with_doc map);
  close_out oc

let to_arg option ?doc key =
  let doc =
    match doc, option.doc with
      Some s, _
    | None, Some s -> "... "^s
    | None, None -> ""
  in
  let f str =
    try
      let json =
        try Yojson.Safe.from_string str
        with Yojson.Json_error msg ->
            try
              Yojson.Safe.from_string (Printf.sprintf "%S" str)
            with Yojson.Json_error _ ->
                json_error msg
      in
      from_json_option [key] option json
    with
      Error e ->
        let msg =
          match e with
          | Json_error msg -> Printf.sprintf "%s: %s" key msg
          | _ -> string_of_error e
        in
        raise (Arg.Bad msg)
  in
  (key, Arg.String f, doc)
