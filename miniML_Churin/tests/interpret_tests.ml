[@@@ocaml.text "/*"]

(** Copyright 2026, [ChurinNick] *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]


open Lambda_lib.Parser
open Lambda_lib.Interpret



let run_test input =
  match parse input with
  | Ok expr -> print_result (eval_expr expr)
  | Error e -> 
      (match e with
       | `Parsing_error msg -> Printf.printf "Parse error: %s\n" msg)

let run_test_with_steps ?(steps = 50_000) input =
  match parse input with
  | Ok expr -> print_result (eval_expr ~steps expr)
  | Error e -> 
      (match e with
       | `Parsing_error msg -> Printf.printf "Parse error: %s\n" msg)



let%expect_test "literals_and_variables" =
  run_test "42";
  run_test "()";
  run_test "x";
  run_test "x_y_123";
  [%expect {|
    Result: 42
    Result: ()
    Error: Unbound variable: x
    Error: Unbound variable: x_y_123
  |}]

let%expect_test "arithmetic" =
  run_test "1 + 2";
  run_test "3 - 5";
  run_test "2 * 3";
  run_test "6 / 2";
  run_test "7 / 0";
  run_test "1 + 2 * 3";
  run_test "(1 + 2) * 3";
  run_test "-5";
  run_test "- -5";
  run_test "1 + 2 * 3 - 4 / 2";
  run_test "((4 + 6) * (1 - 9)) / 10";
  [%expect {|
    Result: 3
    Result: -2
    Result: 6
    Result: 3
    Error: Division by zero
    Result: 7
    Result: 9
    Result: -5
    Result: 5
    Result: 5
    Result: -8
  |}]



let%expect_test "comparisons" =
  run_test "1 = 1";
  run_test "1 = 2";
  run_test "1 <> 1";
  run_test "1 <> 2";
  run_test "1 < 2";
  run_test "2 <= 2";
  run_test "3 > 2";
  run_test "3 >= 3";
  run_test "5 >= 3";
  [%expect {|
    Result: 1
    Result: 0
    Result: 0
    Result: 1
    Result: 1
    Result: 1
    Result: 1
    Result: 1
    Result: 1
  |}]



let%expect_test "if_expressions" =
  run_test "if 1 then 42 else 0";
  run_test "if 0 then 42 else 0";
  run_test "if 5 > 3 then 100 else 200";
  run_test "if 5 < 3 then 100 else 200";
  [%expect {|
    Result: 42
    Result: 0
    Result: 100
    Result: 200
  |}]



let%expect_test "simple_lambdas" =
  run_test "(fun x -> x) 42";
  run_test "(fun x -> x + 1) 5";
  run_test "(fun x -> fun y -> x + y) 3 4";
  run_test "(fun x y -> x + y) 3 4";  
  run_test "let rec a = 1 in a";
  run_test "fix (fun x -> 1)";
  run_test "let rec a = a in a";
  [%expect {|
    Result: 42
    Result: 6
    Result: 7
    Result: 7
    Error: Expected function, got ()
    Error: fix applied to non-function
    Error: Expected function, got ()
  |}]

  let%expect_test "advanced_lambdas" =
  run_test "(fun f -> f 10) (fun n -> n)";
  run_test "((fun x -> fun y -> fun z -> x + y + z) 1 2 3)";
  run_test "(fun f -> fun x -> f (f x)) (fun n -> n * 3) 5";
  [%expect {|
    Result: 10
    Result: 6
    Result: 45
  |}]


let%expect_test "closures" =
  run_test "(let x = 5 in fun y -> x + y) 3";
  run_test "let add = fun x y -> x + y in add 10 20";
  [%expect {|
    Result: 8
    Result: 30
  |}]



let%expect_test "let_bindings" =
  run_test "let x = 5 in x + 1";
  run_test "let x = 5 in let y = 10 in x + y";
  run_test "let x = 5 in let x = 3 in x";  
  [%expect {|
    Result: 6
    Result: 15
    Result: 3
  |}]

let%expect_test "let_with_functions" =
  run_test "let f = fun x -> x * 2 in f 5";
  run_test "let f x = x * 2 in f 5";  
  run_test "let f x y = x + y in f 3 4";
  [%expect {|
    Result: 10
    Result: 10
    Result: 7
  |}]



let%expect_test "factorial" =
  run_test {|
    let rec fact = fun n ->
      if n <= 1 then 1
      else n * fact (n - 1)
    in fact 5
  |};
  run_test {|
    let rec fact n =
      if n <= 1 then 1
      else n * fact (n - 1)
    in fact 10
  |};
  [%expect {|
    Result: 120
    Result: 3628800
  |}]

let%expect_test "fibonacci" =
  run_test {|
    let rec fib = fun n ->
      if n <= 1 then n
      else fib (n - 1) + fib (n - 2)
    in fib 10
  |};
  [%expect {|
    Result: 55
  |}]



let%expect_test "fix_combinator" =
  run_test "fix (fun f -> fun n -> if n <= 1 then 1 else n * f (n - 1)) 5";
  [%expect {|
    Result: 120
  |}]




let%expect_test "builtin_print" =
  run_test "print 42";
  run_test "print_int 42";
  run_test "print (1 + 2)"; 
  [%expect {|
    42
    Result: ()
    42
    Result: ()
    3
    Result: ()
  |}]



let%expect_test "complex_expression" =
  run_test {|
    let compose = fun f g -> fun x -> f (g x) in
    let add1 = fun x -> x + 1 in
    let mul2 = fun x -> x * 2 in
    (compose add1 mul2) 5
  |};
  [%expect {|
    Result: 11
  |}]

let%expect_test "another_complex_expression" =
  run_test {|
    let twice = fun f -> fun x -> f (f x) in
    let add3 = fun x -> x + 3 in
    twice add3 10
  |};
  [%expect {|
    Result: 16
  |}]



let%expect_test "different_errors" =
  run_test "1 + (fun x -> x)";
  run_test "- (fun x -> x)";       
  run_test "let x = 5 in x 3";
  run_test "let rec x = 5 in x ";
  run_test "if () then 1 else 0";
  run_test "if (fun x -> x) then 1 else 0 ";
  [%expect {|
    Error: Expected integer, got <fun x>
    Error: Expected integer, got <fun x>
    Error: Expected function, got 5
    Error: Expected function, got ()
    Error: Invalid condition (expected integer): ()
    Error: Invalid condition (expected integer): <fun x>
  |}]

let%expect_test "unbound_variables" =
  run_test "let x = 5 in x + y";
  run_test "let f x = x + y in f 3";
  [%expect {|
    Error: Unbound variable: y
    Error: Unbound variable: y
  |}]

let%expect_test "division_by_zero" =
  run_test "let x = 0 in 5 / x";
  run_test "(fun x -> 10 / x) 0";
  [%expect {|
    Error: Division by zero
    Error: Division by zero
  |}]

let%expect_test "errors_with_fix" =
  run_test "fix 42";
  run_test "fix (fun x -> 42)";
  run_test "let x = 5 in fix x";
  run_test "fix (fun f -> fun x -> f x) 42";
  [%expect {|
    Error: fix applied to non-function
    Error: fix applied to non-function
    Error: fix applied to non-function
    Error: Step limit exceeded
  |}]




let%expect_test "step_limit" =
  run_test_with_steps ~steps:10 {|
    let rec inf = fun x -> inf x in
    inf 0
  |};
  run_test_with_steps ~steps:1000 {|
    let rec fact = fun n ->
      if n <= 1 then 1
      else n * fact (n - 1)
    in fact 20
  |};
  [%expect {|
    Error: Step limit exceeded
    Result: 2432902008176640000
  |}]

