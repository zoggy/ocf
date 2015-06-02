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

let port = Ocf.int ~doc: "Port number" 80
let host = Ocf.string ~doc: "Host name" "localhost"
let server_options =
  let g = Ocf.group in
  let g = Ocf.add g ["port"] port in
  let g = Ocf.add g ["host"] host in
  g
let all_options =
  let g = Ocf.group in
  Ocf.add_group g ["server"] server_options

let json = {| { server: { port: 8080 , host: "myserver.net" } } |}
let () = Ocf.from_string all_options json
let () = Printf.printf "port=%d, host=%S\n" (Ocf.get port) (Ocf.get host)
let () = print_endline (Ocf.to_string all_options)