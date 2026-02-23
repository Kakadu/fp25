(** Copyright 2021-2026, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Lambda_lib
open Ast

let gen_name =
  let open QCheck.Gen in
  oneof_list [ "x"; "y"; "z"; "a"; "b"; "c"; "x1"; "y_2"; "n'"; "fact"; "fib" ]
;;

let gen_binop =
  let open QCheck.Gen in
  oneof_list [ Add; Sub; Mul; Div ]
;;

let gen_cmpop =
  let open QCheck.Gen in
  oneof_list [ Eq; Neq; Lt; Le; Gt; Ge ]
;;

let gen_expr =
  let open QCheck.Gen in
  let rec expr depth =
    delay (fun () ->
      if depth <= 0
      then
        oneof
          [ map (fun i -> Int i) (int_range (-100) 100); map (fun s -> Var s) gen_name ]
      else (
        let new_depth = depth - 1 in
        oneof_weighted
          [ 3, map (fun i -> Int i) (int_range (-100) 100)
          ; 3, map (fun s -> Var s) gen_name
          ; ( 1
            , map3
                (fun op l r -> Binop (op, l, r))
                gen_binop
                (expr new_depth)
                (expr new_depth) )
          ; ( 1
            , map3
                (fun op l r -> Cmp (op, l, r))
                gen_cmpop
                (expr new_depth)
                (expr new_depth) )
          ; 1, map2 (fun name body -> Abs (name, body)) gen_name (expr new_depth)
          ; 2, map2 (fun e1 e2 -> App (e1, e2)) (expr new_depth) (expr new_depth)
          ; ( 1
            , map3
                (fun c t e -> If (c, t, e))
                (expr new_depth)
                (expr new_depth)
                (expr new_depth) )
          ; ( 1
            , map3
                (fun x e1 e2 -> Let (x, e1, e2))
                gen_name
                (expr new_depth)
                (expr new_depth) )
          ; ( 1
            , map3
                (fun x e1 e2 -> Let_rec (x, e1, e2))
                gen_name
                (expr new_depth)
                (expr new_depth) )
          ]))
  in
  sized (fun n -> expr (min n 5))
;;

let print_expr e = Printast.show e
let arbitrary_expr = QCheck.make ~print:print_expr gen_expr

let test_print_parse_roundtrip =
  QCheck.Test.make
    ~count:100
    ~name:"Roundtrip: Parse(Print(e)) == e"
    arbitrary_expr
    (fun ast ->
       let str = print_expr ast in
       match Parser.parse str with
       | Ok ast' -> ast = ast'
       | Error _ -> false)
;;

let test_printer_safety =
  QCheck.Test.make ~count:100 ~name:"Printer Safety" arbitrary_expr (fun ast ->
    try
      let _ = print_expr ast in
      true
    with
    | _ -> false)
;;

let test_parser_negative =
  let invalid_inputs =
    [ ""; "let x ="; "1 +"; "if true then"; "fun -> x"; "let rec 1 = x" ]
  in
  let gen_invalid = QCheck.Gen.oneof_list invalid_inputs in
  QCheck.Test.make
    ~count:(List.length invalid_inputs)
    ~name:"Negative Parser Tests"
    (QCheck.make ~print:(fun s -> s) gen_invalid)
    (fun str ->
      match Parser.parse str with
      | Error (Parser.Parsing_error _) -> true
      | Ok _ -> false)
;;

let () =
  let tests = [ test_print_parse_roundtrip; test_printer_safety; test_parser_negative ] in
  QCheck_runner.run_tests_main tests
;;
