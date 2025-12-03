open Parser
open Interpreter

let run_interpreter_test input =
  let program = parse_structure_items input in
  match program with
  | Error msg -> Printf.printf "Parse Error: %s\n" msg
  | Ok prog ->
    (match Interpreter.run_program prog with
     | Ok _ -> ()
     | Error err -> Printf.printf "Runtime Error: %s\n" (Interpreter.show_error err))
;;

let%expect_test "interpreter test print int simple" =
  run_interpreter_test "print_int 5";
  [%expect {| 5 |}]
;;

let%expect_test "interpreter test print int expression" =
  run_interpreter_test "print_int (2 + 3 * 4)";
  [%expect {| 14 |}]
;;

let%expect_test "interpreter test let binding and print" =
  run_interpreter_test "let x = 10 in print_int (x * 2)";
  [%expect {| 20 |}]
;;

let%expect_test "interpreter test function definition and application" =
  run_interpreter_test "let f x y = x + y ;; \n    let () = print_int (f 3 4)";
  [%expect {| 7 |}]
;;

let%expect_test "interpreter test recursive function (factorial)" =
  run_interpreter_test
    "let rec fact n = \n\
    \    if n then n * fact (n - 1) else 1 ;; \n\
    \    let () = print_int (fact 2000)";
  [%expect {| 0 |}]
;;

let%expect_test "interpreter test recursive function (fibonacci)" =
  run_interpreter_test
    "let fib n =\n\
    \      let rec fib_iter a b count =\n\
    \        if count then fib_iter b (a + b) (count - 1)\n\
    \        else a\n\
    \      in\n\
    \      fib_iter 0 1 n\n\
    \    ;;\n\
    \    let () = print_int (fib 9)";
  [%expect {| 34 |}]
;;

let%expect_test "lambda binding" =
  run_interpreter_test
    "let add = fun x y z -> x + y + z ;; let () = print_int (add 3 4 5)";
  [%expect {|12|}]
;;

let%expect_test "factorial with fix" =
  run_interpreter_test
    "\n\
    \    let fact = fix (fun slf n -> if n then n * slf (n - 1) else 1) ;;\n\
    \    let () = print_int (fact 10)\n\
    \  ";
  [%expect {| 3628800 |}]
;;

let%expect_test "functions" =
  run_interpreter_test
    "\n\
    \    let add = fun x y -> x + y ;;\n\
    \    let inc = fun x -> add x 1 ;;\n\
    \    let () = print_int (inc 41)\n\
    \  ";
  [%expect {| 42 |}]
;;

let%expect_test "let rec unit binding" =
  run_interpreter_test "let rec () = print_int 42 ;;";
  [%expect
    {| Runtime Error: Error: Type error - Recursive value definition cannot be unit |}]
;;

let%expect_test "unit function test" =
  run_interpreter_test "let rec () = print_int 42";
  [%expect
    {| Runtime Error: Error: Type error - Recursive value definition cannot be unit |}]
;;

let%expect_test "unit value test" =
  run_interpreter_test
    "let printer a b = \n\
    \    let () = print_int a in \n\
    \    let () = print_int b in\n\
    \    () ;; \n\
    \ let () = printer 42 34";
  [%expect {| 4234 |}]
;;

let%expect_test "unbound variable test" =
  run_interpreter_test "let () = print_int x";
  [%expect {| Runtime Error: Error: Unbound variable x |}]
;;

let%expect_test "unop test" =
  run_interpreter_test "let () = print_int (-5 / 2)";
  [%expect {| -2 |}]
;;

let%expect_test "binop with non-integer types" =
  run_interpreter_test "let () = print_int (5 + (fun x -> x))";
  [%expect
    {| Runtime Error: Error: Type error - Cannot apply binary operator to non-integers |}]
;;

let%expect_test "print_endl test" =
  run_interpreter_test "let () = print_endl ()";
  [%expect {| |}]
;;

let%expect_test "unit unop" =
  run_interpreter_test "let () = print_int (-())";
  [%expect {| Runtime Error: Error: Type error - Expected integer for negation |}]
;;

let%expect_test "unit condition" =
  run_interpreter_test "let () = if () then 1 else 0";
  [%expect {| Runtime Error: Error: Type error - Expected integer in if condition |}]
;;

let%expect_test "any pattern in value binding" =
  run_interpreter_test "let _ = print_int 42";
  [%expect {| 42 |}]
;;

let%expect_test "applying argument to function with no parameters" =
  run_interpreter_test "let f = 42 ;; let () = print_int (f 5)";
  [%expect {| Runtime Error: Error: Type error - Cannot apply a non-function |}]
;;
