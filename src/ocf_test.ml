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


type t = {
    t_int : int  [@ocf Ocf.Wrapper.int, 10] [@ocf.label "n"];
    t_list : string list [@ocf Ocf.Wrapper.list Ocf.Wrapper.string, []] ;
  } [@@ocf]
(*
let default_t = { t_int = 10 ; t_list = [] }
let t_wrapper =
  let t_int_wrapper = Ocf.Wrapper.int in
  let t_list_wrapper = Ocf.Wrapper.list Ocf.Wrapper.string in
  let to_j = fun t ->
    `Assoc
      [ "n", t_int_wrapper.Ocf.Wrapper.to_json t.t_int ;
        "t_list", t_list_wrapper.Ocf.Wrapper.to_json t.t_list ;
      ]
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
  let from_assocs map =
    {
     t_int = get t_int_wrapper 10 "n" map ;
     t_list = get t_list_wrapper [] "t_list" map ;
    }
  in
  let from_j = function
    | `Assoc l ->
        let assocs = List.fold_left
          (fun acc (k,v) -> Ocf.SMap.add k v acc) Ocf.SMap.empty l
        in
        from_assocs assocs
    | (json : Yojson.Safe.json) -> Ocf.invalid_value json
  in
  Ocf.Wrapper.wrapper to_j from_j
*)
let print_t t = print_endline
  (Printf.sprintf "{ t_int = %d ; t_list = [%s] }"
    t.t_int (String.concat " ; " t.t_list))

let t_opt = Ocf.option ~cb: (List.iter print_t)
  (Ocf.Wrapper.list t_wrapper) []
let root = Ocf.add root ["t"] t_opt

let () = ignore(Ocf.from_string root
    {| { t : ( { n: 1, t_list: ("hello", "world", "!") }, { n: 100 } ) } |})

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

  