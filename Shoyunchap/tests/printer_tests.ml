open Lambda_lib
open Ast
open Parser
open Printer

let rec random_expr depth =
  let open Random in
  if depth = 0 then
    if bool () then Const (Int (int 10)) else Var (String.make 1 (Char.chr 97))
  else
    match int 5 with
    | 0 -> Const (Int (int 100))
    | 1 -> Var (String.make 1 (Char.chr (97 + int 3)))
    | 2 ->
        let x = String.make 1 (Char.chr (97 + int 3)) in
        Fun (x, random_expr (depth - 1))
    | 3 ->
        App (random_expr (depth - 1), random_expr (depth - 1))
    | _ ->
        BinOp (OpAdd, random_expr (depth - 1), random_expr (depth - 1))

let () =
  Random.init 42;
  for _ = 1 to 100 do
    let e = random_expr 3 in
    let printed = string_of_expr e in
    match parse printed with
    | Ok e' ->
        let printed' = string_of_expr e' in
        (match parse printed' with
         | Ok e'' ->
             let printed'' = string_of_expr e'' in
             if String.equal printed' printed'' then ()
             else
               failwith
                 ( "Roundtrip changed the expression: "
                 ^ printed'
                 ^ " -> "
                 ^ printed'' )
         | Error _ ->
             failwith ("Printer produced unparsable code: " ^ printed'))
    | Error _ ->
        failwith ("Printer produced unparsable code: " ^ printed)
  done
