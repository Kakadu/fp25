open MiniML

let run_expr str =
  match Parser.parse_expression str with
  | Ok ast -> Ast.show_expression ast |> Format.printf "parsed: %s"
  | Error err -> err |> print_endline
;;

let%expect_test _ =
  run_expr "123";
  [%expect {| parsed: 123 |}]
;;

let%expect_test _ =
  run_expr "1 + 2 + 3";
  [%expect
    {|
    parsed: ((1 + 2) + 3) |}]
;;

let%expect_test _ =
  run_expr "1 + 2 * 3";
  [%expect
    {|
    parsed: (1 + (2 * 3)) |}]
;;

let%expect_test _ =
  run_expr "(1, 2, 3)";
  [%expect {| parsed: (1, 2, 3) |}]
;;

let%expect_test _ =
  run_expr "1, 2, 3";
  [%expect {| parsed: (1, 2, 3) |}]
;;

let%expect_test _ =
  run_expr "1 + 2, 3 + 4";
  [%expect
    {|
    parsed: ((1 + 2), (3 + 4)) |}]
;;

let%expect_test _ =
  run_expr "(1 + 2, 3 + 4)";
  [%expect
    {|
    parsed: ((1 + 2), (3 + 4)) |}]
;;

let%expect_test _ =
  run_expr "a :: b :: c :: d";
  [%expect
    {|
    parsed: (a :: (b :: (c :: d))) |}]
;;

let%expect_test _ =
  run_expr "a :: b = c :: d";
  [%expect
    {|
    parsed: ((a :: b) = (c :: d)) |}]
;;

let%expect_test _ =
  run_expr "a * b >= 1 + 2 / 3";
  [%expect
    {|
    parsed: ((a * b) >= (1 + (2 / 3))) |}]
;;

(* *)

let parse_eval_print expr =
  match Parser.parse_expression expr with
  | Error err -> Printf.printf "parsing failed : %s" err
  | Ok ast ->
    (match Interpreter.eval_expr ast with
     | Ok value -> Printf.printf "evaluated : %s" (Interpreter.show_value value)
     | Error err -> Printf.printf "interpreting failed : %s" (Interpreter.show_error err))
;;

let%expect_test _ =
  parse_eval_print "1 + 2 + 3";
  [%expect {| evaluated : 6 |}]
;;

let%expect_test _ =
  parse_eval_print "(1 + 2)";
  [%expect {| evaluated : 3 |}]
;;

let%expect_test _ =
  parse_eval_print "(1 + 2, 2, 3 - 2)";
  [%expect {| evaluated : (3, 2, 1) |}]
;;
