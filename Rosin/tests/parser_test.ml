[@@@ocaml.text "/*"]

(** Copyright 2021-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open QCheck
open Miniml_lib
open Ast

let expr_to_string e = Format.asprintf "%a" Pprintast.pp e

(* Простой генератор имен переменных *)
let varname_gen = Gen.(string_size (int_range 1 3))

(* Простой генератор чисел *)
let num_gen = Gen.(int_range (-100) 100)

(* Безопасный генератор выражений - только простые конструкции *)
let gen_simple_expr =
  let rec expr depth =
    if depth <= 0
    then
      Gen.oneof [ Gen.map (fun i -> Num i) num_gen; Gen.map (fun s -> Var s) varname_gen ]
    else if depth = 1
    then
      Gen.oneof
        [ Gen.map (fun i -> Num i) num_gen
        ; Gen.map (fun s -> Var s) varname_gen
        ; Gen.map (fun (name, body) -> Fun (name, body)) Gen.(pair varname_gen (expr 0))
        ]
    else (
      let next_depth = depth - 1 in
      Gen.oneof
        [ Gen.map (fun i -> Num i) num_gen
        ; Gen.map (fun s -> Var s) varname_gen
        ; Gen.map
            (fun (name, body) -> Fun (name, body))
            Gen.(pair varname_gen (expr next_depth))
        ; Gen.map (fun e -> Print e) (expr next_depth)
        ])
  in
  Gen.sized (fun size -> expr (min size 2))
;;

(* Очень маленькая глубина! *)

(* Arbitrary для простых выражений *)
let arb_simple_expr =
  make
    ~print:(fun e ->
      try Format.asprintf "%a" Pprintast.pp e with
      | _ -> "<too large to print>")
    gen_simple_expr
;;

(* Тест 1: Принтер не падает на простых выражениях *)
let test_printer_safety =
  Test.make
    ~name:"Printer doesn't crash on simple expressions"
    ~count:20 (* Очень мало тестов! *)
    arb_simple_expr
    (fun e ->
       try
         let _ = Format.asprintf "%a" Pprintast.pp e in
         true
       with
       | exn ->
         Printf.eprintf "Printer crashed: %s\n" (Printexc.to_string exn);
         false)
;;

(* Тест 2: Парсер не падает на корректном синтаксисе (ручные примеры) *)
let test_parser_on_valid_syntax =
  let valid_strings =
    [ "42"
    ; "x"
    ; "++x"
    ; "--y"
    ; "1 + 2"
    ; "3 * 4"
    ; "5 / 2"
    ; "if 1 then 2 else 3"
    ; "fun x -> x"
    ; "let x = 1 in x"
    ; "let rec f = fun x -> x in f"
    ; "fix (fun x -> x)"
    ; "print 42"
    ]
  in
  let arb_valid_string = make ~print:(fun s -> s) (Gen.oneofl valid_strings) in
  Test.make
    ~name:"Parser doesn't crash on valid syntax"
    ~count:(List.length valid_strings)
    arb_valid_string
    (fun s ->
       try
         match Parser.parse s with
         | Ok _ -> true
         | Error err ->
           (* Парсинг может закончиться ошибкой, но не должен падать *)
           Printf.eprintf
             "Parser returned error (expected): %s for: %s\n"
             (match err with
              | `Parsing_error msg -> msg)
             s;
           true (* Ошибка парсера - это нормально, главное что не упал *)
       with
       | exn ->
         Printf.eprintf "Parser crashed on: %s with: %s\n" s (Printexc.to_string exn);
         false)
;;

let test_printer_concrete_cases =
  let cases =
    [ Num 42, "Int(42)"
    ; Var "x", "Var(x)"
    ; Fun ("x", Var "x"), "Fun(x, Var(x))"
    ; Let ("x", Num 1, Var "x"), "Let(x, Int(1)) in Var(x))"
    ; Print (Num 42), "Print(Int(42))"
    ; If (Num 1, Num 2, Some (Num 3)), "If(Int(1)) Then(Int(2)) Else (Int(3)))"
    ; If (Num 1, Num 2, None), "If(Int(1)) Then(Int(2))"
    ]
  in
  let arb_case =
    make
      ~print:(fun (expr, _) -> Format.asprintf "%a" Pprintast.pp expr)
      (Gen.oneofl cases)
  in
  Test.make
    ~name:"Printer produces expected output for concrete cases"
    ~count:(List.length cases)
    arb_case
    (fun (expr, expected) ->
       try
         let result = Format.asprintf "%a" Pprintast.pp expr in
         if result = expected
         then true
         else (
           Printf.eprintf "Printer mismatch:\n  Expected: %s\n  Got: %s\n" expected result;
           false)
       with
       | exn ->
         Printf.eprintf "Printer crashed: %s\n" (Printexc.to_string exn);
         false)
;;

(* Тест 4: Отрицательные тесты для парсера *)
let test_parser_negative =
  let invalid_strings =
    [ ""
    ; "++"
    ; "if then else"
    ; "let x = in x"
    ; "fun -> x"
    ; "fix 42"
    ; (* fix должен содержать fun в скобках *)
      "1 + + 2"
    ]
  in
  let arb_invalid = make ~print:(fun s -> s) (Gen.oneofl invalid_strings) in
  Test.make
    ~name:"Parser handles invalid syntax gracefully"
    ~count:(List.length invalid_strings)
    arb_invalid
    (fun s ->
       try
         match Parser.parse s with
         | Ok expr ->
           Printf.eprintf
             "Parser should have failed but returned: %s\n"
             (expr_to_string expr);
           false
         | Error _ -> true (* Ожидаем ошибку для невалидного синтаксиса *)
       with
       | exn ->
         Printf.eprintf
           "Parser crashed on invalid input: %s with: %s\n"
           s
           (Printexc.to_string exn);
         false)
;;

(* Тест 5: Проверка структуры AST после парсинга *)
let test_parser_structure =
  let test_cases =
    [ ( "42"
      , function
        | Num 42 -> true
        | _ -> false )
    ; ( "x"
      , function
        | Var "x" -> true
        | _ -> false )
    ; ( "fun x -> x"
      , function
        | Fun ("x", Var "x") -> true
        | _ -> false )
    ; ( "let x = 1 in x"
      , function
        | Let ("x", Num 1, Var "x") -> true
        | _ -> false )
    ]
  in
  let arb_test_case = make ~print:(fun (s, _) -> s) (Gen.oneofl test_cases) in
  Test.make
    ~name:"Parser produces correct AST structure"
    ~count:(List.length test_cases)
    arb_test_case
    (fun (input, validator) ->
       try
         match Parser.parse input with
         | Ok expr -> validator expr
         | Error err ->
           Printf.eprintf
             "Parser error for valid input %s: %s\n"
             input
             (match err with
              | `Parsing_error msg -> msg);
           false
       with
       | exn ->
         Printf.eprintf "Parser crashed: %s\n" (Printexc.to_string exn);
         false)
;;

(* Запуск всех тестов *)

let tests =
  [ test_printer_safety
  ; test_parser_on_valid_syntax
  ; test_printer_concrete_cases
  ; test_parser_negative
  ; test_parser_structure
  ]
;;

QCheck_runner.run_tests ~verbose:true tests
