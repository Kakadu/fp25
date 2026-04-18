[@@@ocaml.text "*/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "*/*"]

open QCheck
module Parser = Mini_ml_lib.Parser
module Ast = Mini_ml_lib.Ast

let parse_and_print input =
  (match Parser.parse_line input with
   | Parsed (a, _) -> Ast.show_ast_verbose a
   | ParseError e -> Parser.show_error e)
  |> print_endline
;;

(* Test if valid expressions are parsed *)

let%expect_test "Correct parsing" =
  "  (((45 \t  +   x)   + \t  ((213))))  " |> parse_and_print;
  [%expect "Add(Add(45, x), 213)"];
  "\n x \t\n - \t\n 5 \t\n" |> parse_and_print;
  [%expect "Sub(x, 5)"];
  "x001 + x002 - x003 + x004 + complexVariable222 - simpleVariable333" |> parse_and_print;
  [%expect
    "Sub(Add(Add(Sub(Add(x001, x002), x003), x004), complexVariable222), \
     simpleVariable333)"];
  "(false) && (true) \t = \t (1) > (3)" |> parse_and_print;
  [%expect "And(false, Greater(Equal(true, 1), 3))"];
  "(4) mod (5 / 2)" |> parse_and_print;
  [%expect "Mod(4, Div(5, 2))"];
  "(2 + 2) * 2 <> 2 + 2 * 2" |> parse_and_print;
  [%expect "NotEqual(Mul(Add(2, 2), 2), Add(2, Mul(2, 2)))"];
  "(1, 2, 3, (false, true), (1 - 2))" |> parse_and_print;
  [%expect "Tuple(1, 2, 3, Tuple(false, true), Sub(1, 2))"];
  "fun x->fun y->fun z->(x + y + z)" |> parse_and_print;
  [%expect "Abs(x, Abs(y, Abs(z, Add(Add(x, y), z))))"];
  "(fun x -> (2 * x + 1), fun x -> (x / 2 - 1))" |> parse_and_print;
  [%expect "Tuple(Abs(x, Add(Mul(2, x), 1)), Abs(x, Sub(Div(x, 2), 1)))"];
  "if (var > 0) then var else -var" |> parse_and_print;
  [%expect "IfThenElse(Greater(var, 0), var, Neg(var))"];
  "if var1 then 1 else \n if var2 then 2 else \n if var3 then 3 else 4" |> parse_and_print;
  [%expect "IfThenElse(var1, 1, IfThenElse(var2, 2, IfThenElse(var3, 3, 4)))"];
  "if a then 1 else 2 + if b then true else false" |> parse_and_print;
  [%expect "Add(IfThenElse(a, 1, 2), IfThenElse(b, true, false))"];
  "fun a -> -(if a then 1 else 2)" |> parse_and_print;
  [%expect "Abs(a, Neg(IfThenElse(a, 1, 2)))"];
  "let a = 1 in let b = 2 in let c = 3 in (a + b + c)" |> parse_and_print;
  [%expect "Let(a, 1, Let(b, 2, Let(c, 3, Add(Add(a, b), c))))"]
;;

(* Test if invalid expressions are not parsed *)

let%expect_test "Invalid expressions" =
  "( )" |> parse_and_print;
  [%expect {|Syntax error: ")"|}];
  "(1 $ 2" |> parse_and_print;
  [%expect {|Syntax error: "$ 2"|}];
  "var & mask)" |> parse_and_print;
  [%expect {|Unparsed symbols: "& mask)"|}];
  "var += 1" |> parse_and_print;
  [%expect {|Unparsed symbols: "+= 1"|}];
  "(567 / 3) + (567 % 3" |> parse_and_print;
  [%expect {|Unparsed symbols: "+ (567 % 3"|}];
  "(call 1invalid2identifier)" |> parse_and_print;
  [%expect {|Syntax error: "1invalid2identifier)"|}];
  "+ 1 2 3" |> parse_and_print;
  [%expect {|Syntax error: "+ 1 2 3"|}];
  "(1, 2, 3 helloWorld)" |> parse_and_print;
  [%expect {|Syntax error: ", 2, 3 helloWorld)"|}];
  "fun x y ->" |> parse_and_print;
  [%expect {|Syntax error: "fun x y ->"|}];
  "if a <> 0 then 1" |> parse_and_print;
  [%expect {|Syntax error: "if a <> 0 then 1"|}];
  "let in = 1 in in" |> parse_and_print;
  [%expect {|Syntax error: "let in = 1 in in"|}];
  "let mod = 1 in var0 + var1" |> parse_and_print;
  [%expect {|Syntax error: "let mod = 1 in var0 + var1"|}];
  "let false = 1 in true" |> parse_and_print;
  [%expect {|Syntax error: "let false = 1 in true"|}];
  "let true = 1 in false" |> parse_and_print;
  [%expect {|Syntax error: "let true = 1 in false"|}];
  "(1, 2, (if, then))" |> parse_and_print;
  [%expect {|Syntax error: ", 2, (if, then))"|}];
  "(then, 3)" |> parse_and_print;
  [%expect {|Syntax error: "then, 3)"|}];
  "let var = 1" |> parse_and_print;
  [%expect {|Syntax error: "let var = 1"|}]
;;

let same lhs rhs =
  match Parser.parse_line lhs with
  | ParseError _ -> false
  | Parsed (a, _) ->
    (match Parser.parse_line rhs with
     | ParseError _ -> false
     | Parsed (b, _) -> a = b)
;;

(* Test if syntax sugar for application & abstraction arguments works *)

let%test "Abstraction syntax sugar" =
  same "fun x -> (fun y -> (fun z -> (x + y + z)))" "fun x y z -> (x + y + z)"
;;

let%test "Application syntax sugar" =
  same "(((func arg0) (-arg1)) (!arg2))" "(func arg0 -arg1 !arg2)"
;;

(* Test if apply operator works *)

let%test "Application operator 1" =
  same "(func arg0 arg1 arg2)" "func @@ arg0 @@ arg1 @@ arg2"
;;

let%test "Application operator 2" =
  same "(func (func1 arg1) arg)" "func @@ (func1 @@ arg1) @@ arg"
;;

let%test "Application operator 3" =
  same "((func arg) (func1 arg1))" "(func @@ arg) @@ (func1 @@ arg1)"
;;

(* QCheck tests *)

let gen_real_name =
  let open Gen in
  let capital_alpha = char_range 'A' 'Z' in
  let alphanum = oneof [ char_range 'a' 'z'; char_range 'A' 'Z'; char_numeral ] in
  let ident =
    map
      (fun chars -> chars |> List.to_seq |> String.of_seq)
      (let* first = capital_alpha in
       let* rest = list_size (int_range 1 10) alphanum in
       return (first :: rest))
  in
  map (fun s -> Ast.Real s) ident
;;

let gen_any_name =
  let open Gen in
  let wildcard = return Ast.Wildcard in
  oneof [ gen_real_name; wildcard ]
;;

let gen_unary_operation = Gen.oneof [ Gen.return Ast.Neg; Gen.return Ast.Not ]

let gen_binary_operation =
  Gen.oneof
    (List.map
       Gen.return
       [ Ast.Add
       ; Ast.Sub
       ; Ast.Mul
       ; Ast.Div
       ; Ast.Mod
       ; Ast.And
       ; Ast.Or
       ; Ast.Equal
       ; Ast.NotEqual
       ; Ast.Less
       ; Ast.LessEqual
       ; Ast.Greater
       ; Ast.GreaterEqual
       ])
;;

let gen_rec_flag = Gen.oneof_weighted [ 5, Gen.return Ast.Let; 1, Gen.return Ast.LetRec ]

let gen_ast_depth =
  let open Gen in
  fix (fun gen size ->
    if size <= 0
    then
      oneof
        [ return Ast.Unit
        ; map (fun i -> Ast.Int i) (int_range 0 100000)
        ; map (fun b -> Ast.Bool b) bool
        ; map (fun n -> Ast.Var n) gen_real_name
        ]
    else (
      let sub = gen (size / 2) in
      let sub_small = gen (size / 3) in
      let sub_tiny = oneof [ gen 1; gen 2 ] in
      oneof_weighted
        [ 1, map (fun i -> Ast.Int i) (int_range 0 100000)
        ; 1, map (fun b -> Ast.Bool b) bool
        ; 1, map (fun n -> Ast.Var n) gen_real_name
        ; 2, map2 (fun op expr -> Ast.UnaryOp (op, expr)) gen_unary_operation sub
        ; ( 4
          , map3
              (fun op left right -> Ast.BinaryOp (op, left, right))
              gen_binary_operation
              sub
              sub )
        ; ( 2
          , Gen.map3
              (fun first second rest -> Ast.Tuple (first, second, rest))
              sub_tiny
              sub_tiny
              (list_size (int_range 0 5) sub_tiny) )
        ; 4, map2 (fun arg expr -> Ast.Abstraction (arg, expr)) gen_any_name sub
        ; 4, map2 (fun func arg -> Ast.Application (func, arg)) sub_small sub
        ; ( 2
          , map3
              (fun cond on_true on_false -> Ast.IfThenElse (cond, on_true, on_false))
              sub_small
              sub
              sub )
        ; ( 4
          , map4
              (fun rf name expr1 expr2 -> Ast.LetExpr (rf, name, expr1, expr2))
              gen_rec_flag
              gen_any_name
              sub_small
              sub )
        ]))
;;

let gen_ast limit = Gen.sized (fun size -> gen_ast_depth (min size limit))
let gen_expr = Gen.map Ast.show_ast (gen_ast 5)

let roundtrip_test_1 =
  Test.make
    ~name:"Parser round-trip 1"
    ~count:50
    (make ~print:Ast.show_ast (gen_ast 7))
    (fun ast ->
      let string = Ast.show_ast ast in
      match Parser.parse_line string with
      | Parsed (a, _) -> ast = a
      | ParseError _ -> false)
;;

let roundtrip_test_2 =
  Test.make
    ~name:"Parser round-trip 2"
    ~count:50
    (make ~print:Fun.id gen_expr)
    (fun expr ->
       match Parser.parse_line expr with
       | Parsed (a, _) -> expr = Ast.show_ast a
       | ParseError _ -> false)
;;

let parser_idempotent_test =
  Test.make
    ~name:"Parser idempotence"
    ~count:20
    (make ~print:Fun.id gen_expr)
    (fun expr ->
       let ast1 = Parser.parse_line expr in
       let ast2 = Parser.parse_line expr in
       ast1 = ast2)
;;

let printer_idempotent_test =
  Test.make
    ~name:"Printer idempotence"
    ~count:20
    (make ~print:Ast.show_ast (gen_ast 10))
    (fun ast ->
      let string1 = Ast.show_ast ast in
      let string2 = Ast.show_ast ast in
      string1 = string2)
;;

let%test "QCheck test" =
  let res =
    QCheck_runner.run_tests
      ~long:true
      ~verbose:true
      [ roundtrip_test_1
      ; roundtrip_test_2
      ; parser_idempotent_test
      ; printer_idempotent_test
      ]
  in
  res = 0
;;
