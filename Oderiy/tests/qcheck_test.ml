(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: CC0-1.0 *)

open Mml.Ast
open Mml.Parser
open Mml.Print
open Mml.Interpret

let test_roundtrip_int () =
  let test n =
    let ast = Int n in
    let printed = to_string ast in
    match parse printed with
    | Ok parsed ->
      (match parsed with
       | Int m -> m = n
       | Neg (Int m) -> -m = n
       | _ -> false)
    | Error _ -> false
  in
  List.for_all test [ 0; 1; 42; 100; 999 ]
;;

let test_roundtrip_var () =
  let test name =
    let ast = Var name in
    let printed = to_string ast in
    match parse printed with
    | Ok (Var parsed_name) -> parsed_name = name
    | _ -> false
  in
  List.for_all test [ "x"; "y"; "foo"; "bar123"; "_tmp" ]
;;

let test_roundtrip_binop () =
  let test_op op =
    let ast = Bin (op, Int 3, Int 5) in
    let printed = to_string ast in
    match parse printed with
    | Ok (Bin (parsed_op, Int 3, Int 5)) -> parsed_op = op
    | _ -> false
  in
  test_op Add && test_op Sub && test_op Mul && test_op Div
;;

let test_roundtrip_fun () =
  let ast = Fun ("x", Bin (Add, Var "x", Int 1)) in
  let printed = to_string ast in
  match parse printed with
  | Ok _ -> true
  | Error _ -> false
;;

let test_roundtrip_let () =
  let ast = Let ("x", Int 10, Bin (Add, Var "x", Int 5)) in
  let printed = to_string ast in
  match parse printed with
  | Ok _ -> true
  | Error _ -> false
;;

let test_roundtrip_if () =
  let ast = If (Bin (Gt, Int 5, Int 3), Int 1, Int 0) in
  let printed = to_string ast in
  match parse printed with
  | Ok _ -> true
  | Error _ -> false
;;

let test_roundtrip_letrec () =
  let ast =
    LetRec
      ( "fact"
      , Fun
          ( "n"
          , If
              ( Bin (Leq, Var "n", Int 1)
              , Int 1
              , Bin (Mul, Var "n", App (Var "fact", Bin (Sub, Var "n", Int 1))) ) )
      , App (Var "fact", Int 5) )
  in
  let printed = to_string ast in
  match parse printed with
  | Ok _ -> true
  | Error _ -> false
;;

(* Test that parser rejects malformed input *)
let test_parse_invalid () =
  let invalid_inputs = [ "123abc"; "***"; "fun ->"; "if then" ] in
  List.for_all
    (fun input ->
      match parse input with
      | Error _ -> true
      | Ok _ -> false)
    invalid_inputs
;;

let test_print_valid () =
  let exprs =
    [ Int 42
    ; Var "x"
    ; Bin (Add, Int 1, Int 2)
    ; Fun ("x", Var "x")
    ; App (Var "f", Var "x")
    ; Let ("x", Int 5, Var "x")
    ; If (Int 1, Int 2, Int 3)
    ; Neg (Int 5)
    ]
  in
  List.for_all
    (fun ast ->
      let printed = to_string ast in
      match parse printed with
      | Ok _ -> true
      | Error _ -> false)
    exprs
;;

let test_print_deterministic () =
  let exprs =
    [ Int 42; Var "test"; Bin (Mul, Int 3, Int 7); Fun ("a", Bin (Add, Var "a", Int 10)) ]
  in
  List.for_all (fun ast -> to_string ast = to_string ast) exprs
;;

let test_parse_invalid () =
  let invalid_inputs = [ "123abc"; "***"; "fun ->"; "if then"; "let x ="; "let x = " ] in
  List.for_all
    (fun input ->
      match parse input with
      | Error _ -> true
      | Ok _ -> false)
    invalid_inputs
;;

(* Interpreter property tests *)
let test_interp_arithmetic () =
  let test_cases =
    [ Int 42, 42
    ; Bin (Add, Int 10, Int 20), 30
    ; Bin (Sub, Int 50, Int 30), 20
    ; Bin (Mul, Int 6, Int 7), 42
    ; Bin (Div, Int 100, Int 5), 20
    ; Neg (Int 5), -5
    ; Neg (Neg (Int 10)), 10
    ]
  in
  List.for_all
    (fun (expr, expected) ->
      match run 1000 expr with
      | Ok result -> result = expected
      | Error _ -> false)
    test_cases
;;

let test_interp_comparison () =
  let test_cases =
    [ Bin (Lt, Int 3, Int 5), 1
    ; Bin (Lt, Int 5, Int 3), 0
    ; Bin (Leq, Int 5, Int 5), 1
    ; Bin (Eq, Int 42, Int 42), 1
    ; Bin (Eq, Int 10, Int 20), 0
    ; Bin (Geq, Int 10, Int 5), 1
    ; Bin (Gt, Int 20, Int 10), 1
    ]
  in
  List.for_all
    (fun (expr, expected) ->
      match run 1000 expr with
      | Ok result -> result = expected
      | Error _ -> false)
    test_cases
;;

let test_interp_functions () =
  let id_func = Fun ("x", Var "x") in
  let inc_func = Fun ("x", Bin (Add, Var "x", Int 1)) in
  let test_cases =
    [ App (id_func, Int 42), 42
    ; App (inc_func, Int 10), 11
    ; Let ("x", Int 5, Var "x"), 5
    ; Let ("x", Int 10, Bin (Add, Var "x", Int 5)), 15
    ]
  in
  List.for_all
    (fun (expr, expected) ->
      match run 1000 expr with
      | Ok result -> result = expected
      | Error _ -> false)
    test_cases
;;

let test_interp_conditionals () =
  let test_cases =
    [ If (Int 1, Int 42, Int 0), 42
    ; If (Int 0, Int 10, Int 20), 20
    ; If (Bin (Gt, Int 5, Int 3), Int 100, Int 200), 100
    ]
  in
  List.for_all
    (fun (expr, expected) ->
      match run 1000 expr with
      | Ok result -> result = expected
      | Error _ -> false)
    test_cases
;;

let test_interp_recursion () =
  let fact5 =
    LetRec
      ( "fact"
      , Fun
          ( "n"
          , If
              ( Bin (Leq, Var "n", Int 1)
              , Int 1
              , Bin (Mul, Var "n", App (Var "fact", Bin (Sub, Var "n", Int 1))) ) )
      , App (Var "fact", Int 5) )
  in
  match run 10000 fact5 with
  | Ok 120 -> true
  | _ -> false
;;

let test_interp_errors () =
  let error_cases =
    [ Var "undefined"
    ; Bin (Div, Int 10, Int 0)
    ; App (Int 5, Int 10)
    ; Neg (Fun ("x", Var "x"))
    ]
  in
  List.for_all
    (fun expr ->
      match run 1000 expr with
      | Error _ -> true
      | Ok _ -> false)
    error_cases
;;

let test_interp_step_limit () =
  let infinite_loop =
    LetRec ("loop", Fun ("x", App (Var "loop", Var "x")), App (Var "loop", Int 0))
  in
  match run 100 infinite_loop with
  | Error `Steps_exceeded -> true
  | _ -> false
;;

let () =
  let tests =
    [ "roundtrip: integers", test_roundtrip_int
    ; "roundtrip: variables", test_roundtrip_var
    ; "roundtrip: binary ops", test_roundtrip_binop
    ; "roundtrip: functions", test_roundtrip_fun
    ; "roundtrip: let", test_roundtrip_let
    ; "roundtrip: if", test_roundtrip_if
    ; "roundtrip: let rec", test_roundtrip_letrec
    ; "print: produces valid syntax", test_print_valid
    ; "print: deterministic", test_print_deterministic
    ; "parse: invalid input", test_parse_invalid
    ; "interp: arithmetic operations", test_interp_arithmetic
    ; "interp: comparison operations", test_interp_comparison
    ; "interp: functions and let", test_interp_functions
    ; "interp: conditionals", test_interp_conditionals
    ; "interp: recursion (factorial)", test_interp_recursion
    ; "interp: error handling", test_interp_errors
    ; "interp: step limit", test_interp_step_limit
    ]
  in
  let total = List.length tests in
  let passed =
    List.fold_left
      (fun acc (name, test) ->
        Printf.printf "Testing: %s ... %!" name;
        if test ()
        then (
          Printf.printf "PASS\n%!";
          acc + 1)
        else (
          Printf.printf "FAIL\n%!";
          acc))
      0
      tests
  in
  Printf.printf "\nPassed %d/%d tests\n%!" passed total;
  if passed < total then exit 1
;;
