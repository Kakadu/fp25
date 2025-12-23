[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Lambda_lib
open Parser
open Interpret
open Pprintast

let rec fact n = if n = 1 then 1 else n * fact (n - 1)
let%test _ = fact 5 = 120

(* test number parsing *)
(* successfully parsed numbers *)
let num str =
  match Angstrom.parse_string parse_number ~consume:Angstrom.Consume.All str with
  | Result.Ok x -> Result.Ok x
  | Error _ -> Result.Error (`Parsing_error "Failed to parse number")
;;

let%test _ = Result.get_ok (parse "100") = Ast.Const 100
let%test _ = Result.get_ok (parse "1") = Ast.Const 1
let%test _ = Result.get_ok (parse "-1") = Ast.Const (-1)
let%test _ = Result.get_ok (parse "    -102929") = Ast.Const (-102929)
let%test _ = Result.get_ok (parse "(1)") = Ast.Const 1

(* no success *)
let%test _ = Result.get_error (num "-") = `Parsing_error "Failed to parse number"

(* let%test _ = Result.get_error (num "1-000") = `Parsing_error "Failed to parse" *)
let%test _ = Result.get_error (num "--1") = `Parsing_error "Failed to parse number"

(* test ident parsing *)
(* successfully parsed identifiers *)
let ident str =
  match Angstrom.parse_string parse_varname ~consume:Angstrom.Consume.All str with
  | Result.Ok x -> Result.Ok x
  | Error _ -> Result.Error (`Parsing_error "Failed to parse")
;;

let%test _ = Result.get_ok (ident "aaa") = "aaa"
let%test _ = Result.get_ok (ident "1a") = "1a"
let%test _ = Result.get_ok (ident "  a1") = "a1"

(* no success *)
let%test _ = Result.get_error (ident "1") = `Parsing_error "Failed to parse"
let%test _ = Result.get_error (ident "") = `Parsing_error "Failed to parse"
let%test _ = Result.get_error (ident "let") = `Parsing_error "Failed to parse"

(* test parser of arithmetics *)
let algebr str =
  match Angstrom.parse_string parse_expr ~consume:Angstrom.Consume.All str with
  | Result.Ok x -> Result.Ok x
  | Error _ -> Result.Error (`Parsing_error "Failed to parse ident")
;;

let%test _ =
  Result.get_ok (algebr "1+2") = Ast.Binexpr (Ast.Plus, Ast.Const 1, Ast.Const 2)
;;

let%test _ =
  Result.get_ok (algebr "2*a") = Ast.Binexpr (Ast.Mul, Ast.Const 2, Ast.Ident "a")
;;

let%test _ =
  Result.get_ok (algebr "1*(2+3)")
  = Ast.Binexpr (Ast.Mul, Ast.Const 1, Ast.Binexpr (Ast.Plus, Ast.Const 2, Ast.Const 3))
;;

let%test _ =
  Result.get_ok (algebr "a*b+c")
  = Ast.Binexpr
      (Ast.Plus, Ast.Binexpr (Ast.Mul, Ast.Ident "a", Ast.Ident "b"), Ast.Ident "c")
;;

let%test _ = Result.get_ok (algebr "a") = Ast.Ident "a"

(* test some comparison *)
let compr str =
  match Angstrom.parse_string parse_expr ~consume:Angstrom.Consume.All str with
  | Result.Ok x -> Result.Ok x
  | Error _ -> Result.Error (`Parsing_error "Failed to parse ident")
;;

let%test _ =
  Result.get_ok (compr " 1 =    2") = Ast.Binexpr (Ast.Eq, Ast.Const 1, Ast.Const 2)
;;

let%test _ =
  Result.get_ok (compr "1 * a <2")
  = Ast.Binexpr (Ast.Le, Ast.Binexpr (Ast.Mul, Ast.Const 1, Ast.Ident "a"), Ast.Const 2)
;;

let%test _ =
  Result.get_ok (algebr "1 + 2") = Ast.Binexpr (Ast.Plus, Ast.Const 1, Ast.Const 2)
;;

(* test conditionals *)
let ite str =
  match Angstrom.parse_string parse_expr ~consume:Angstrom.Consume.All str with
  | Result.Ok x -> Result.Ok x
  | Error _ -> Result.Error (`Parsing_error "Failed to parse ident")
;;

let%test _ =
  Result.get_ok (ite " if 1 then 2 else 3")
  = Ast.Ite (Ast.Const 1, Ast.Const 2, Ast.Const 3)
;;

let%test _ =
  Result.get_ok (ite "if if 0 then 2 else 3 then 1 else 10")
  = Ast.Ite (Ast.Ite (Ast.Const 0, Ast.Const 2, Ast.Const 3), Ast.Const 1, Ast.Const 10)
;;

(* test abstractions *)
let%test _ = Result.get_ok (parse "fun x -> a") = Ast.Abs ("x", Ast.Ident "a")

let%test _ =
  Result.get_ok (parse "fun x -> fun k -> x + k")
  = Ast.Abs ("x", Ast.Abs ("k", Ast.Binexpr (Ast.Plus, Ast.Ident "x", Ast.Ident "k")))
;;

let%test _ =
  Result.get_ok (parse "fun x -> if x = 0 then 1 else 2")
  = Ast.Abs
      ( "x"
      , Ast.Ite
          (Ast.Binexpr (Ast.Eq, Ast.Ident "x", Ast.Const 0), Ast.Const 1, Ast.Const 2) )
;;

(* test application *)
let%test _ = Result.get_ok (parse "fact 10") = Ast.App (Ast.Var ("fact", Ast.Const 10))

let%test _ =
  Result.get_ok (parse "(fun x -> x 1) (fun y -> y)")
  = Ast.App
      (Ast.Fun ("x", Ast.App (Var ("x", Ast.Const 1)), Ast.Abs ("y", Ast.Ident "y")))
;;

let%test _ =
  Result.get_ok (parse "if n < 2 then 1 else fact (n - 1)")
  = Ast.Ite
      ( Ast.Binexpr (Ast.Le, Ast.Ident "n", Ast.Const 2)
      , Ast.Const 1
      , Ast.App (Var ("fact", Ast.Binexpr (Ast.Minus, Ast.Ident "n", Ast.Const 1))) )
;;

let%test _ = Result.get_ok (parse "ABCDEFGHGREW") = Ast.Ident "ABCDEFGHGREW"

let%test _ =
  Result.get_ok (parse "f (n) + 2")
  = Ast.Binexpr (Ast.Plus, Ast.App (Var ("f", Ast.Ident "n")), Ast.Const 2)
;;

let%test _ =
  Result.get_ok (eval init 10 (Ast.Binexpr (Ast.Plus, Ast.Const 1, Ast.Const 2))) = EVal 3
;;

let%test _ =
  Result.get_error (eval init 10 (Ast.Binexpr (Ast.Div, Ast.Const 1, Ast.Const 0)))
  = "div by zero"
;;

let%test _ =
  Result.get_ok
    (eval
       init
       10
       (Ast.Binexpr
          (Ast.Plus, Ast.Binexpr (Ast.Mul, Ast.Const 3, Ast.Const 3), Ast.Const 2)))
  = EVal 11
;;

let%test _ =
  Result.get_error
    (eval
       init
       10
       (Ast.Binexpr
          (Ast.Plus, Ast.Binexpr (Ast.Div, Ast.Const 3, Ast.Const 0), Ast.Const 2)))
  = "div by zero"
;;

let%test _ =
  Result.get_ok (eval init 10 (Ast.Binexpr (Ast.Minus, Ast.Const 10, Ast.Const 5)))
  = EVal 5
;;

let%test _ =
  Result.get_ok
    (eval
       init
       1000
       (Ast.App
          (Ast.Fun ("x", Ast.Binexpr (Ast.Plus, Ast.Ident "x", Ast.Const 1), Ast.Const 100))))
  = EVal 101
;;

let%test _ =
  Result.get_ok (eval init 1000 (Result.get_ok (parse "(fun x y -> (x - y)) 10 20")))
  = EVal (-10)
;;

let%test _ =
  Result.get_ok (eval init 1000 (Result.get_ok (parse "let fact n = 10  in fact 1")))
  = EVal 10
;;

let%test _ =
  Result.get_ok (eval init 10 (Result.get_ok (parse "let a = 10 in a + 1"))) = EVal 11
;;

let%test _ =
  Result.get_ok
    (eval
       init
       1000
       (Result.get_ok (parse "let rec f n = if n = 3 then 10 else f (n - 1) in f 4")))
  = EVal 10
;;

let%test _ =
  Result.get_ok
    (eval
       init
       100
       (Result.get_ok
          (parse "let rec fact n = if n = 1 then 1 else n * fact (n - 1) in fact 6")))
  = EVal 720
;;

let%test _ =
  Result.get_ok (parse "(n - (1))") = Ast.Binexpr (Ast.Minus, Ast.Ident "n", Ast.Const 1)
;;

let%test _ =
  Result.get_ok (parse "((n) - (1))") = Ast.Binexpr (Ast.Minus, Ast.Ident "n", Ast.Const 1)
;;

let%test _ =
  Result.get_ok (Parser.parse "(fact (6))") = Ast.App (Ast.Var ("fact", Ast.Const 6))
;;

let%test _ =
  Result.get_ok
    (eval
       init
       1000
       (Result.get_ok
          (parse
             (pprint
                (Result.get_ok
                   (parse
                      "let rec fact n = if n < 1 then 1 else n * fact (n - 1) in    \n\
                      \ \r \t                      fact 5"))))))
  = EVal 120
;;

let%test _ =
  Result.get_ok (parse "(a 1) 2")
  = Ast.App (Ast.Application (Ast.Var ("a", Ast.Const 1), Ast.Const 2))
;;

Printf.printf
  "%s\n"
  (Pprintast.pprint
     (Result.get_ok
        (parse
           "let n = 10 in\n\
           \   let ifn a b = let z = print a in print b in\n\
           \   let thenn = 100 in\n\
           \   let elsen = 1000 in\n\
           \   ifn thenn 1000")))

let%test _ =
  Result.get_ok (parse "i e t")
  = Ast.App (Ast.Application (Ast.Var ("i", Ast.Ident "e"), Ast.Ident "t"))
;;

let%test _ = Result.get_ok (parse "i_ 1") = Ast.App (Ast.Var ("i_", Ast.Const 1))
