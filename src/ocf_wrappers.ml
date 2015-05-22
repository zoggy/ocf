(** *)

open Ocf_types

let wrapper to_json from_json = { to_json ; from_json }

let int =
  let to_j n = `Int n in
  let from_j = function
    `Int n -> n
  | (`Intlit s)
  | (`String s) as json ->
      begin try int_of_string s with _ -> invalid_value json end
  | json -> invalid_value json
  in
  wrapper to_j from_j

let float =
  let to_j x = `Float x in
  let from_j = function
    `Float x -> x
  | `Int n -> float n
  | (`Intlit s)
  | (`String s) as json ->
      begin try float_of_string s with _ -> invalid_value json end
  | json -> invalid_value json
  in
  wrapper to_j from_j

let string =
  let to_j x = `String x in
  let from_j = function
    `String s -> s
  | json -> invalid_value json
  in
  wrapper to_j from_j

let list w =
  let to_j l = `List (List.map w.to_json l) in
  let from_j = function
  | `List l
  | `Tuple l -> List.map w.from_json l
  | `Null -> []
  | json -> invalid_value json
  in
  wrapper to_j from_j

let option w =
  let to_j = function None -> `Null | Some x -> w.to_json x in
  let from_j = function
    `Null -> None
  | x -> Some (w.from_json x)
  in
  wrapper to_j from_j

let pair w1 w2 =
  let to_j (v1, v2) = `Tuple [w1.to_json v1 ; w2.to_json v2] in
  let from_j = function
    `List [v1 ; v2]
  | `Tuple [v1 ; v2] -> (w1.from_json v1, w2.from_json v2)
  | json -> invalid_value json
  in
  wrapper to_j from_j

let triple w1 w2 w3 =
  let to_j (v1, v2, v3) =
    `Tuple [w1.to_json v1 ; w2.to_json v2 ; w3.to_json v3]
  in
  let from_j = function
    `List [v1 ; v2 ; v3]
  | `Tuple [v1 ; v2 ; v3] ->
      (w1.from_json v1, w2.from_json v2, w3.from_json v3)
  | json -> invalid_value json
  in
  wrapper to_j from_j

