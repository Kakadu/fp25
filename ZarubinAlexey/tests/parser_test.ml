(** Copyright 2021-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: CC0-1.0 *)

(** Тесты парсера: парсим строку и печатаем AST *)

open Base
open Lambda_lib

(* удобный шорткат для печати AST *)
let show_ast = Print.print_ast

(* парсим строку или падаем в тесте, если парсер вернул ошибку *)
let parse_exn s =
  match Parser.parse s with
  | Ok e -> e
  | Error _ -> raise (Failure "parser error in test")
;;

let%expect_test "ints and vars" =
  Stdlib.print_endline (show_ast (parse_exn "42"));
  [%expect {|(Int 42)|}];
  Stdlib.print_endline (show_ast (parse_exn "   0   "));
  [%expect {|(Int 0)|}];
  Stdlib.print_endline (show_ast (parse_exn "x"));
  [%expect {|(Var x)|}];
  Stdlib.print_endline (show_ast (parse_exn "foo"));
  [%expect {|(Var foo)|}]
;;

let%expect_test "simple binops" =
  Stdlib.print_endline (show_ast (parse_exn "1+2"));
  [%expect {|(Binop (Add, (Int 1), (Int 2)))|}];
  Stdlib.print_endline (show_ast (parse_exn "1 - 2"));
  [%expect {|(Binop (Sub, (Int 1), (Int 2)))|}];
  Stdlib.print_endline (show_ast (parse_exn "2*3+4"));
  [%expect {|
(Binop (Add, (Binop (Mul, (Int 2), (Int 3))), (Int 4)))
|}]
;;

let%expect_test "if / let / let rec fact" =
  Stdlib.print_endline (show_ast (parse_exn "if x = 0 then 1 else 2"));
  [%expect {|
(If ((Binop (Eq, (Var x), (Int 0))), (Int 1), (Int 2)))
|}];
  Stdlib.print_endline (show_ast (parse_exn "let x = 5 in x + 1"));
  [%expect {|
(Let (x, (Int 5), (Binop (Add, (Var x), (Int 1)))))
|}];
  Stdlib.print_endline
    (show_ast
       (parse_exn "let rec fact n = if n = 0 then 1 else n * fact (n - 1) in fact 5"));
  [%expect
    {|
(Let_rec (fact, n,
   (If ((Binop (Eq, (Var n), (Int 0))), (Int 1),
      (Binop (Mul, (Var n),
         (App ((Var fact), (Binop (Sub, (Var n), (Int 1)))))))
      )),
   (App ((Var fact), (Int 5)))))
|}]
;;
