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

let lid loc s = Location.mkloc (Longident.parse s) loc

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

let type_decl_mapper mapper decl =
  match decl.ptype_kind with
  | Ptype_record fields when has_ocf_attribute decl.ptype_attributes ->
      begin
        decl
      end

  | _ -> default_mapper.type_declaration mapper decl

let ocf_mapper argv =
  { default_mapper with
    type_declaration = type_decl_mapper ;
  }

let () = register "ocf" ocf_mapper

