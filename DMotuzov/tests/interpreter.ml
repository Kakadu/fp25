open Base
open DMotuzov_lib.Parser
open DMotuzov_lib.Interpreter

(* Вспомогательная функция для печати value *)
let string_of_value = function
  | ValInt n -> string_of_int n
  | ValUnit -> "()"
  | ValFun (_, _, _) -> "<fun>"
  | RecClosure (_, _, _, _) -> "<rec-fun>"
  | Builtin _ -> "<builtin_fun>"
;;

(* Печать окружения *)
let print_env env =
  Map.iteri env ~f:(fun ~key ~data ->
    Format.printf "val %s = %s\n" key (string_of_value data))
;;

(* Запуск интерпретатора на строке кода *)
let run input =
  match parse input with
  | Ok ast ->
    (match Inter.eval_program empty_with_builtins ast with
     | Ok env -> print_env env
     | Error e ->
       let err_str =
         match e with
         | TypeError -> "Type error"
         | DivisionByZero -> "Division by zero"
         | NoVariable id -> "No such variable: " ^ id
       in
       Format.printf "Interpreter error: %s\n" err_str)
  | Error err -> Format.printf "Parse error: %s\n" err
;;

(* ==================== тесты ==================== *)

let%expect_test "simple let binding" =
  run "let a = 4 + 4;;";
  [%expect {|
  val a = 8
  val print_int = <builtin_fun>
  |}]
;;

let%expect_test "let in expression" =
  run "let f = let x = 3 in x + 2;;";
  [%expect {|
  val f = 5
  val print_int = <builtin_fun>
  |}]
;;

let%expect_test "conditional expression" =
  run "let a = if 1 then 10 else 20;;";
  [%expect {|
  val a = 10
  val print_int = <builtin_fun>
  |}]
;;

let%expect_test "conditional expression with bin op" =
  run "let a =20 + if 1 then 10 else 20;;";
  [%expect {|
  val a = 30
  val print_int = <builtin_fun>
  |}]
;;

let%expect_test "nested if" =
  run "let x = if 1 then if 0 then 2 else 3 else 4 ;;";
  [%expect {|
  val print_int = <builtin_fun>
  val x = 3
  |}]
;;

let%expect_test "nested if" =
  run "let x = if 1 then if 0 then 2 else 3 else 4 ;;let y = 2 + x;;";
  [%expect {|
    val print_int = <builtin_fun>
    val x = 3
    val y = 5 |}]
;;

"let x = (fun x y -> x + y) 3 4 ;;"

let%expect_test "application" =
  run "let x = (fun x y -> x + y) 3 4 ;;";
  [%expect {|
    val print_int = <builtin_fun>
    val x = 7 |}]
;;

let%expect_test "fact" =
  run
    "let f = let rec fact = fun n -> if n then n * fact (n - 1) else 1 in fact ;; let y \
     = f 10;;";
  [%expect
    {|
    val f = <rec-fun>
    val print_int = <builtin_fun>
    val y = 3628800 |}]
;;

"let rec fib = fun n -> if n then if n - 1 then fib (n - 1) + fib (n - 2) else 1 else 0 \
 ;;"

let%expect_test "fib" =
  run
    "let rec fib = fun n -> if n then if n - 1 then fib (n - 1) + fib (n - 2) else 1 \
     else 0 ;; let y = fib 10;;";
  [%expect {|
    val fib = <rec-fun>
    val print_int = <builtin_fun>
    val y = 55 |}]
;;

let%expect_test "local let rec fact" =
  run
    "let f =\n\
    \       let rec fact = fun n ->\n\
    \         if n then n * fact (n - 1) else 1\n\
    \       in fact\n\
    \     ;; let y = f 5 ;;";
  [%expect {|
    val f = <rec-fun>
    val print_int = <builtin_fun>
    val y = 120 |}]
;;

let%expect_test "fix" =
  run "let id = fix (fun f -> fun x -> x) ;;let y = id 10;;";
  [%expect {|
    val id = <rec-fun>
    val print_int = <builtin_fun>
    val y = 10 |}]
;;

let%expect_test "fix factorial" =
  run
    "let fact = fix (fun f n -> if n then n * (f (n - 1)) else 1) ;;\n\
    \       let y = fact 6 ;;";
  [%expect
    {|
    val fact = <rec-fun>
    val print_int = <builtin_fun>
    val y = 720 |}]
;;

let%expect_test "print_int" =
  run "let y = 40;;let x = print_int 1 y ((fun x -> x ) 5) (if 1 then 10 else 20);;";
  [%expect
    {|
    1
    40
    5
    10
    val print_int = <builtin_fun>
    val x = <builtin_fun>
    val y = 40 |}]
;;
