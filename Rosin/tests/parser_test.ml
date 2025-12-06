[@@@ocaml.text "/*"]

(** Copyright 2021-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open QCheck
open Miniml_lib
open Ast
open Gen

let expr_to_string e = Format.asprintf "%a" Pprintast.pp e
let varname_gen = string_size (int_range 1 3)
let int_gen = int_range (-100) 100
let unop_gen = oneofl [ Inc; Dec ]
let binop_gen = oneofl [ Plus; Minus; Mult; Div ]

let gen_expr =
  let rec expr depth =
    if depth <= 0
    then oneof [ map (fun i -> Num i) int_gen; map (fun s -> Var s) varname_gen ]
    else (
      let next_depth = depth - 1 in
      frequency
        [ 3, map (fun i -> Num i) int_gen
        ; 3, map (fun s -> Var s) varname_gen
        ; 2, map (fun (op, e) -> Unop (op, e)) (pair unop_gen (expr next_depth))
        ; ( 2
          , map
              (fun (op, e1, e2) -> Binop (op, e1, e2))
              (triple binop_gen (expr next_depth) (expr next_depth)) )
        ; ( 1
          , map
              (fun (c, t, e) -> If (c, t, e))
              (triple (expr next_depth) (expr next_depth) (option (expr next_depth))) )
        ; ( 1
          , map
              (fun (name, body) -> Ast.Fun (name, body))
              (pair varname_gen (expr next_depth)) )
        ; ( 1
          , map
              (fun (name, rhs, body) -> Let (name, rhs, body))
              (triple varname_gen (expr next_depth) (expr next_depth)) )
        ; ( 1
          , map
              (fun (name, rhs, body) -> Letrec (name, rhs, body))
              (triple varname_gen (expr next_depth) (expr next_depth)) )
        ; ( 1
          , map
              (fun (name, body) -> Fix (Fun (name, body)))
              (pair varname_gen (expr next_depth)) )
        ; 2, map (fun (f, x) -> App (f, x)) (pair (expr next_depth) (expr next_depth))
        ; 1, map (fun e -> Print e) (expr next_depth)
        ])
  in
  sized (fun size -> expr (min size 3))
;;

let arb_expr =
  make
    ~print:(fun e ->
      try expr_to_string e with
      | _ -> "<too large to print>")
    gen_expr
;;

let test_printer_safety =
  Test.make ~name:"Printer safety" ~count:50 arb_expr (fun e ->
    try
      let _ = expr_to_string e in
      true
    with
    | exn ->
      Printf.eprintf "Printer crashed: %s\n" (Printexc.to_string exn);
      false)
;;

let test_simple_roundtrip =
  let simple_cases =
    [ "42", Num 42
    ; "x", Var "x"
    ; "++x", Unop (Inc, Var "x")
    ; "--y", Unop (Dec, Var "y")
    ; "1 + 2", Binop (Plus, Num 1, Num 2)
    ; "3 * 4", Binop (Mult, Num 3, Num 4)
    ; "if 1 then 2", If (Num 1, Num 2, None)
    ; "if 1 then 2 else 3", If (Num 1, Num 2, Some (Num 3))
    ; "fun x -> x", Fun ("x", Var "x")
    ; "let x = 1 in x", Let ("x", Num 1, Var "x")
    ; "print 42", Print (Num 42)
    ]
  in
  let arb_simple = make ~print:(fun (s, _) -> s) (oneofl simple_cases) in
  Test.make
    ~name:"Simple round-trip"
    ~count:(List.length simple_cases)
    arb_simple
    (fun (expected_str, expected_expr) ->
       try
         let printed = expr_to_string expected_expr in
         if printed <> expected_str
         then (
           Printf.eprintf
             "Print mismatch:\n  Expected: %s\n  Got: %s\n"
             expected_str
             printed;
           false)
         else (
           match Parser.parse printed with
           | Ok parsed_expr -> expected_expr = parsed_expr
           | Error err ->
             Printf.eprintf
               "Parse error: %s\n"
               (match err with
                | `Parsing_error msg -> msg);
             false)
       with
       | exn ->
         Printf.eprintf "Exception: %s\n" (Printexc.to_string exn);
         false)
;;

let test_parser_on_valid =
  let valid_strings =
    [ "42"
    ; "x"
    ; "++x"
    ; "--y"
    ; "1 + 2"
    ; "3 * 4"
    ; "5 / 2"
    ; "6 - 1"
    ; "if 1 then 2"
    ; "if 1 then 2 else 3"
    ; "fun x -> x"
    ; "let x = 1 in x"
    ; "let rec f = fun x -> x in f"
    ; "fix (fun x -> x)"
    ; "print 42"
    ; "f x"
    ; "f x y"
    ]
  in
  let arb_valid = make ~print:(fun s -> s) (oneofl valid_strings) in
  Test.make
    ~name:"Parser on valid syntax"
    ~count:(List.length valid_strings)
    arb_valid
    (fun s ->
       try
         match Parser.parse s with
         | Ok _ -> true
         | Error err ->
           Printf.eprintf
             "Parser error (expected for some): %s -> %s\n"
             s
             (match err with
              | `Parsing_error msg -> msg);
           true
       with
       | exn ->
         Printf.eprintf "Parser crashed: %s -> %s\n" s (Printexc.to_string exn);
         false)
;;

let test_parser_negative =
  let invalid_strings =
    [ ""
    ; "++"
    ; "if then else"
    ; "let x = in x"
    ; "fun -> x"
    ; "fix 42"
    ; "1 + + 2"
    ; "let rec = 1 in x"
    ; "print"
    ]
  in
  let arb_invalid = make ~print:(fun s -> s) (oneofl invalid_strings) in
  Test.make
    ~name:"Parser negative tests"
    ~count:(List.length invalid_strings)
    arb_invalid
    (fun s ->
       try
         match Parser.parse s with
         | Ok expr ->
           Printf.eprintf "Should have failed: %s -> %s\n" s (expr_to_string expr);
           false
         | Error _ -> true
       with
       | exn ->
         Printf.eprintf "Parser crashed: %s -> %s\n" s (Printexc.to_string exn);
         false)
;;

let test_parser_ast =
  let test_cases =
    [ ( "42"
      , function
        | Num 42 -> true
        | _ -> false )
    ; ( "x"
      , function
        | Var "x" -> true
        | _ -> false )
    ; ( "++x"
      , function
        | Unop (Inc, Var "x") -> true
        | _ -> false )
    ; ( "1 + 2"
      , function
        | Binop (Plus, Num 1, Num 2) -> true
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
  let arb_case = make ~print:(fun (s, _) -> s) (oneofl test_cases) in
  Test.make
    ~name:"Parser AST structure"
    ~count:(List.length test_cases)
    arb_case
    (fun (input, validator) ->
       try
         match Parser.parse input with
         | Ok expr -> validator expr
         | Error err ->
           Printf.eprintf
             "Parse error: %s -> %s\n"
             input
             (match err with
              | `Parsing_error msg -> msg);
           false
       with
       | exn ->
         Printf.eprintf "Exception: %s\n" (Printexc.to_string exn);
         false)
;;

let tests =
  [ test_printer_safety
  ; test_simple_roundtrip
  ; test_parser_on_valid
  ; test_parser_negative
  ; test_parser_ast
  ]
;;

QCheck_runner.run_tests ~verbose:true tests
