open Base
open DMotuzov_lib.Parser
open DMotuzov_lib.Interpreter

(* Вспомогательная функция для печати value *)
let string_of_value = function
  | ValInt n -> string_of_int n
  | ValUnit -> "()"
  | ValFun (_, _, _) -> "<fun>"
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
    (match Inter.eval_program EvalEnv.empty ast with
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
  |}]
;;

let%expect_test "let in expression" =
  run "let f = let x = 3 in x + 2;;";
  [%expect {|
  val f = 5
  |}]
;;

let%expect_test "conditional expression" =
  run "let a = if 1 then 10 else 20;;";
  [%expect {|
  val a = 10
  |}]
;;

let%expect_test "conditional expression with bin op" =
  run "let a =20 + if 1 then 10 else 20;;";
  [%expect {|
  val a = 30
  |}]
;;

let%expect_test "nested if" =
  run "let x = if 1 then if 0 then 2 else 3 else 4 ;;";
  [%expect {|
  val x = 3
  |}]
;;
