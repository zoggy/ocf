Ocf
===

OCaml library to read and write configuration files in JSON syntax.


### Installation

````
make all install
````
This installs the `ocf` package.

### Example

````ocaml
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
````
This will read options from the given JSON string `json`; then `port` and
`host` options contain the value provided in the JSON string.
The last line dumps the configuration with current values. The result is:
````
port=8080, host="myserver.net"
{
  "server": {
    "port": "Port number",
    "port": 8080,
    "host": "Host name",
    "host": "myserver.net"
  }
}

````
