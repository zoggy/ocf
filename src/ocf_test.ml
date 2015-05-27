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

let () =
  try ignore(Ocf.add root ["foo" ; "bar" ; "x"] int_opt)
  with Ocf.Error e -> prerr_endline (Ocf.string_of_error e)

let () =
  try ignore(Ocf.from_string root
    "{ foo: { bar : { x: \"hello\"}}}")
  with Ocf.Error e -> prerr_endline (Ocf.string_of_error e)

let options =
  [ Ocf.to_arg int_opt "-x" ;
    Ocf.to_arg string_opt "-str" ;
  ]

let () = Arg.parse options
  (fun _ -> ())
    (Printf.sprintf "Usage: %s [options]\nwhere options are:" Sys.argv.(0))

  