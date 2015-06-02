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

(** *)

let int_opt = Ocf.int ~doc: "this is x"
  ~cb: (fun n -> prerr_endline ("x="^(string_of_int n))) 0
let float_opt = Ocf.float 3.14
let string_opt = Ocf.string ~doc: "string option"
  ~cb: prerr_endline "hello world!"
let opt_opt = Ocf.option_ Ocf.Wrapper.int None

let group = Ocf.group
let group = Ocf.add group ["x"] int_opt
let group = Ocf.add group ["x_float"] float_opt
let group = Ocf.add group ["msg"] string_opt
let group = Ocf.add group ["option"] opt_opt

let root = Ocf.group
let root = Ocf.add_group root ["foo" ; "bar"] group

let str = Ocf.to_string root
let () = print_endline str
let () = Ocf.from_string root str


type ('a,'b) t = {
    t_int : int  [@ocf Ocf.Wrapper.int, 10] [@ocf.label "n"];
    t_list : 'a list [@ocf Ocf.Wrapper.list, []] ;
    t_list2 : 'a list [@ocf.wrapper Ocf.Wrapper.list] [@ocf.default []] ;
    t_par: 'b ;
    t_cons : 'b option  [@ocf.wrapper Ocf.Wrapper.option] ;
  } [@@ocf]

let print_t t = print_endline
  (Printf.sprintf "{ t_int = %d ; t_list = [%s] }"
    t.t_int (String.concat " ; " t.t_list) )

let t_opt = Ocf.option ~cb: (List.iter print_t)
  (Ocf.Wrapper.list(t_wrapper ~t_par: 1 ~t_cons: None
     Ocf.Wrapper.string Ocf.Wrapper.int )) []
let root = Ocf.add root ["t"] t_opt

let () =
  (*try*) ignore(Ocf.from_string root
     {| { t : ( { n: 1, t_list: ("hello", "world", "!") }, { n: 100 } ) } |})
(*  with Ocf.Error e -> prerr_endline (Ocf.string_of_error e)*)

let () = print_endline (Ocf.to_string root)
let () =
  try ignore(Ocf.add root ["foo" ; "bar" ; "x"] int_opt)
  with Ocf.Error e -> prerr_endline (Ocf.string_of_error e)
let () =
  try ignore(Ocf.from_string root
    {| { foo: { bar : { x: "hello"}}} |})
  with Ocf.Error e -> prerr_endline (Ocf.string_of_error e)

let options =
  [ Ocf.to_arg int_opt "-x" ;
    Ocf.to_arg string_opt "-str" ;
  ]

let () = Arg.parse options
  (fun _ -> ())
    (Printf.sprintf "Usage: %s [options]\nwhere options are:" Sys.argv.(0))

  