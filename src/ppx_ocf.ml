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

(** Ppx processor for Ocf. *)

open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

module SMap = Map.Make(String)

let lid loc s = Location.mkloc (Longident.parse s) loc
let apply ?loc e args = Ast_helper.Exp.apply ?loc e
  (List.map (fun e -> ("", e)) args)
let mk_string loc s = Exp.constant ~loc (Const_string (s, None))
let noloc_lid s = Location.mknoloc (Longident.parse s)
let cons loc = lid loc "::"
let empty_list = Ast_helper.Exp.construct (noloc_lid "[]") None
let mk_list loc l = List.fold_right
  (fun e acc -> Ast_helper.Exp.construct (cons loc)
     (Some (Ast_helper.Exp.tuple [ e ; acc ])))
  l empty_list

(*c==v=[List.list_remove_doubles]=1.0====*)
let list_remove_doubles ?(pred=(=)) l =
  List.fold_left
    (fun acc e -> if List.exists (pred e) acc then acc else e :: acc)
    []
    (List.rev l)
(*/c==v=[List.list_remove_doubles]=1.0====*)


let error loc msg = raise (Location.Error (Location.error ~loc msg))
let kerror loc = Printf.ksprintf (error loc)

let ocf_att_prefix = "ocf"
let len_ocf_att_prefix = String.length ocf_att_prefix

let get_ocf_attr = function
  | s when s = ocf_att_prefix -> Some ""
  | s ->
    let len = String.length s in
    if len < len_ocf_att_prefix + 1 then
      None
    else
      if String.sub s 0 len_ocf_att_prefix = ocf_att_prefix
        && String.get s len_ocf_att_prefix = '.'
      then
        Some (String.sub s (len_ocf_att_prefix + 1)
         (len - (len_ocf_att_prefix + 1)))
      else
        None

let has_ocf_attribute attrs =
  List.exists
    (fun (att,_) -> get_ocf_attr att.txt <> None)
    attrs

type field =
  { name : string loc ;
    label : string loc ;
    doc: expression option ;
    params : string list ;
    wrapper : expression option ;
    default : expression option ;
  }

let params_of_type_params l =
  let f ct acc =
    match ct.ptyp_desc with
      Ptyp_var s -> s :: acc
    | _ -> acc
  in
  list_remove_doubles (List.fold_right f l [])

let attribute name atts =
  try
    Some (List.find(fun ({txt},_) -> txt = name) atts)
  with Not_found -> None

let mk_field l =
  let label =
    match attribute (ocf_att_prefix^".label")
      l.pld_type.ptyp_attributes
    with
    | None -> l.pld_name
    | Some (_,PStr [
          {
            pstr_desc = Pstr_eval
               ({ pexp_desc = Pexp_constant (Const_string (label, _));
                  pexp_loc }, _)
          }
        ]) ->
        { txt = label ; loc = pexp_loc }
    | Some (x,_) ->
        kerror x.loc
          "Invalid expression for %s.label; a string is expected"
          ocf_att_prefix
  in
  let doc = match attribute (ocf_att_prefix^".doc")
      l.pld_type.ptyp_attributes
    with
    | None -> None
    | Some (_,PStr [{pstr_desc = Pstr_eval (e,_)}]) ->
        Some e
    | Some (x,_) ->
        kerror x.loc
          "Invalid expression for %s.doc; an expression is expected"
          ocf_att_prefix
  in
  let (wrapper, default) =
    let atts = l.pld_type.ptyp_attributes in
    match attribute ocf_att_prefix atts with
    | None ->
        begin
          let wrapper =
            match attribute (ocf_att_prefix^".wrapper") atts with
              None -> None
            | Some (_,PStr [ { pstr_desc = Pstr_eval (e,_) } ]) -> Some e
            | Some (x,_) ->
                kerror x.loc
                  "Invalid payload for %s.wrapper; an expression is expected"
                  ocf_att_prefix
          in
          let default =
            match attribute (ocf_att_prefix^".default") atts with
              None -> None
            | Some (_,PStr [ { pstr_desc = Pstr_eval (e,_) } ]) -> Some e
            | Some (x,_) ->
                kerror x.loc
                  "Invalid payload for %s.wrapper; an expression is expected"
                  ocf_att_prefix

          in
          (wrapper, default)
        end
    | Some (_,PStr [ { pstr_desc = Pstr_eval (e,_) } ]) ->
        begin
          match e.pexp_desc with
          | Pexp_tuple[wrapper;default] ->
              (Some wrapper, Some default)
          | _ ->
              kerror e.pexp_loc
                "Invalid expression; a pair of expressions is expected"
        end
    | Some (x,_) ->
        kerror x.loc
          "Invalid expression for %s; a pair of expressions is expected"
          ocf_att_prefix
  in
  let params =
    match l.pld_type.ptyp_desc with
      Ptyp_constr (_,params) -> params_of_type_params params
    | Ptyp_var s -> [s]
    | _ -> []
  in
  { name = l.pld_name ;
    label ; doc ; params ; wrapper ; default
  }

let mk_default decl fields =
  let f fd =
    let expr =
      match fd.default with
        Some def -> def
      | None -> Exp.ident (lid fd.name.loc fd.name.txt)
    in
    (lid fd.name.loc fd.name.txt, expr)
  in
  let record = Exp.record (List.map f fields) None in
  let pat = Pat.var
    (Location.mkloc ("default_"^decl.ptype_name.txt) decl.ptype_loc)
  in
  let expr = List.fold_right
    (fun fd expr ->
       match fd.default with
       | Some _ -> expr
       | None ->
           let pat = Pat.var fd.name in
           Exp.fun_ fd.name.txt None pat expr
    )
    fields record
  in
  Vb.mk pat expr

let mk_wrapper decl fields =
  let params = params_of_type_params
    (List.map fst decl.ptype_params)
  in
  let w_name fd = Printf.sprintf "__wrapper_%s" fd.name.txt in
  let w_lid fd = Exp.ident (lid fd.name.loc (w_name fd)) in

  let mk_wrapper expr fd =
    let pat = Pat.var { loc = fd.name.loc ; txt = w_name fd } in
    let app =
      match fd.params, fd.wrapper with
      | [], None
      | _::_::_, None ->
          kerror fd.name.loc
            "Missing Ocf wrapper for field %s" fd.name.txt
      | [], Some e -> e
      | [param], None -> Exp.ident (lid fd.name.loc ("wrapper_"^param))
      | params, Some e ->
          apply ~loc: fd.name.loc e
            (List.map
             (fun p -> Exp.ident (lid fd.name.loc ("wrapper_"^p)))
               fd.params)
    in
    [%expr let [%p pat] = [%e app] in [%e expr] ]
  in
  let to_json_exprs =
    let f fd =
      let to_json =
        [%expr [%e w_lid fd].Ocf.Wrapper.to_json ?with_doc]
      in
      let fd_exp = Exp.field
        [%expr t] (lid fd.name.loc (fd.name.txt))
      in
      let assoc =
        [%expr
          ([%e mk_string fd.label.loc fd.label.txt],
           [%e to_json ] [%e fd_exp] )
        ]
      in
      let e =
        match fd.doc with
          None -> [%expr [ [%e assoc] ] ]
        | Some exp_doc ->
            [%expr
              match with_doc with
              | Some true ->
                  [
                    [%e mk_string fd.label.loc fd.label.txt],
                    `String [%e exp_doc] ;
                    [%e assoc] ;
                  ]
              | _ -> [ [%e assoc] ]
            ]
      in
      e
    in
    mk_list decl.ptype_loc (List.map f fields)
  in
  let expr_from_assocs =
    let f fd =
      let expr_v =
        let exp_field =
          Exp.field [%expr def] (lid fd.name.loc (fd.name.txt))
        in
        [%expr
          let default =
            match def with
            | Some def -> [%e exp_field]
            | None ->
                [%e match fd.default with
                | Some def -> def
                | None -> Exp.ident (lid fd.name.loc fd.name.txt)
                ]
          in
          get [%e w_lid fd] default
                    [%e mk_string fd.label.loc fd.label.txt] map
        ]
      in
      (lid fd.name.loc fd.name.txt, expr_v)
    in
    Exp.record (List.map f fields) None
  in
  let expr = [%expr
      let to_j ?with_doc t =
        `Assoc (List.flatten [%e to_json_exprs])
      in
      let get w def label map =
        match Ocf.SMap.find label map with
        | exception Not_found -> def
        | json ->
            try w.Ocf.Wrapper.from_json json
            with
            | Ocf.Error e ->
                Ocf.error_at_path [label] e
            | e ->
                Ocf.exn_at_path [label] e
      in
      let from_assocs map def = [%e expr_from_assocs] in
      let from_j ?def = function
      | `Assoc l ->
          begin
            let assocs = List.fold_left
              (fun acc (k,v) -> Ocf.SMap.add k v acc) Ocf.SMap.empty l
            in
            from_assocs assocs def
          end
      | (json : Yojson.Safe.json) -> Ocf.invalid_value json
      in
      Ocf.Wrapper.make to_j from_j
    ]
  in
  let expr = List.fold_left mk_wrapper expr fields in
  let expr =
    let f param expr =
      let pat = Pat.var { loc = decl.ptype_loc ; txt = "wrapper_"^param } in
      [%expr fun [%p pat] -> [%e expr] ]
    in
    List.fold_right f params expr
  in
  let expr =
    let f fd expr =
      match fd.default with
      | Some _ -> expr
      | None ->
          let pat = Pat.var fd.name in
          Exp.fun_ fd.name.txt None pat expr
    in
    List.fold_right f fields expr
  in
  let pat = Pat.var
    (Location.mkloc (decl.ptype_name.txt^"_wrapper") decl.ptype_loc)
  in
  Vb.mk pat expr

let generate decl =
  match decl.ptype_kind with
  | Ptype_record fields ->
      begin
        let fields = List.map mk_field fields in
        let default = mk_default decl fields in
        let wrapper = mk_wrapper decl fields in
        [ default ; wrapper ]
      end
  | _ ->
      kerror decl.ptype_loc
        "Only record types can have @@%s attribute" ocf_att_prefix

let fold_structure acc item =
  match item.pstr_desc with
  | (Pstr_type l) ->
      let type_decls = List.filter
        (fun decl -> has_ocf_attribute decl.ptype_attributes) l
      in
      let bindings = List.flatten (List.map generate type_decls) in
      begin
        match bindings with
          [] -> item :: acc
        | _ ->
            let decl = {
                pstr_desc = Pstr_value (Recursive, bindings) ;
                pstr_loc = item.pstr_loc ;
              }
            in
            decl :: item :: acc
      end
  | _ -> item :: acc

let structure_mapper mapper structure =
  let structure = List.fold_left fold_structure [] structure in
  default_mapper.structure mapper (List.rev structure)

let ocf_mapper argv =
  { default_mapper with
    structure = structure_mapper ;
  }

let () = register "ocf" ocf_mapper

