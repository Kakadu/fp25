[@@@ocaml.text "/*"]

(** Copyright 2021-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Miniml_lib
open Ast
open QCheck.Gen

let expr_to_string e = Format.asprintf "%a" Pprintast.pp e

let varname_gen =
  oneof
    [ pure "x"
    ; pure "y"
    ; pure "z"
    ; pure "a"
    ; pure "b"
    ; pure "c"
    ; pure "f"
    ; pure "g"
    ; pure "h"
    ; pure "n"
    ; pure "m"
    ; pure "p"
    ; pure "q"
    ]
;;

let int_gen = int_range (-100) 100
let unop_gen = oneofl [ Inc; Dec ]
let binop_gen = oneofl [ Plus; Minus; Mult; Div; Equal; Less; More ]

let gen_expr =
  let rec expr depth =
    if depth <= 0
    then oneof [ map (fun i -> Num i) int_gen; map (fun s -> Var s) varname_gen ]
    else (
      let next_depth = depth - 1 in
      frequency
        [ 3, map (fun i -> Num i) int_gen
        ; 3, map (fun s -> Var s) varname_gen
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
              (fun (name, body) -> Fun (name, body))
              (pair varname_gen (expr next_depth)) )
        ; ( 1
          , map
              (fun (name, rhs, body) -> Let (name, rhs, body))
              (triple varname_gen (expr next_depth) (expr next_depth)) )
        ; ( 1
          , map
              (fun (name, rhs, body) -> Letrec (name, rhs, body))
              (triple varname_gen (expr next_depth) (expr next_depth)) )
        ; 2, map (fun (f, x) -> App (f, x)) (pair (expr next_depth) (expr next_depth))
        ])
  in
  sized (fun size -> expr (min size 4))
;;

let arb_expr =
  QCheck.make
    ~print:(fun e ->
      try expr_to_string e with
      | _ -> "<too large to print>")
    gen_expr
;;

let arb_program = QCheck.make (map expr_to_string gen_expr)

let test_printer_safety =
  QCheck.Test.make ~name:"Printer safety" ~count:50 arb_expr (fun e ->
    try
      let _ = expr_to_string e in
      true
    with
    | exn ->
      Printf.eprintf "Printer crashed: %s\n" (Printexc.to_string exn);
      false)
;;

let test_roundtrip =
  QCheck.Test.make ~name:"Simple round-trip" ~count:1000 arb_program (fun expr_str ->
    try
      match Parser.parse expr_str with
      | Error (`Parsing_error _) -> true
      | Ok ast ->
        let printed1 = expr_to_string ast in
        (match Parser.parse printed1 with
         | Error (`Parsing_error _) -> false
         | Ok ast2 ->
           let printed2 = expr_to_string ast2 in
           printed1 = printed2)
    with
    | exn ->
      Printf.eprintf "Exception in idempotent test: %s\n" (Printexc.to_string exn);
      false)
;;

let test_parser_negative =
  let invalid_strings =
    [ ""
    ; "++"
    ; "if then else"
    ; "let x = in x"
    ; "fun -> x"
    ; "1 + + 2"
    ; "let rec = 1 in x"
    ]
  in
  let arb_invalid = QCheck.make ~print:(fun s -> s) (oneofl invalid_strings) in
  QCheck.Test.make
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

let tests = [ test_printer_safety; test_roundtrip; test_parser_negative ];;

QCheck_runner.run_tests ~verbose:true tests
