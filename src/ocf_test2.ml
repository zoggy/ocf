
let port = Ocf.int ~desc: "Port number" 80
let host = Ocf.string ~desc: "Host name" "localhost"
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