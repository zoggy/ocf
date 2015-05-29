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


type 'a t = {
    t_int : int  [@ocf Ocf.Wrapper.int, 10] [@ocf.label "n"];
    t_list : 'a list [@ocf Ocf.Wrapper.list, []] ;
  } [@@ocf]

let print_t t = print_endline
  (Printf.sprintf "{ t_int = %d ; t_list = [%s] }"
    t.t_int (String.concat " ; " t.t_list) )

let t_opt = Ocf.option ~cb: (List.iter print_t)
  (Ocf.Wrapper.list(t_wrapper Ocf.Wrapper.string )) []
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

  