open DMotuzov_lib.Parser
open DMotuzov_lib.Prettyprinter

let run_pp input =
  match parse input with
  | Ok ast -> Format.asprintf "%a" pp_prog ast
  | Error err -> "Parse error: " ^ err
;;

let%expect_test "fib" =
  print_endline
    (run_pp
       "let rec fib = fun n -> if n then if n - 1 then fib (n - 1) + fib (n - 2) else 1 \
        else 0           ;; let y = fib 10;;");
  [%expect
    {|
    let rec fib = fun n -> if n then if (n - 1) then (fib (n - 1) + fib (n - 2)) else 1 else 0;;
    let y = fib 10;;
    |}]
;;

let%expect_test "fact" =
  print_endline (run_pp "let rec fact = fun n -> if n then n * fact (n - 1) else 1 ;;");
  [%expect
    {|
    let rec fact = fun n -> if n then (n * fact (n - 1)) else 1;;
    |}]
;;

let%expect_test "let_rec" =
  print_endline
    (run_pp
       "let f = let rec fact = fun n -> if n then n * fact (n - 1) else 1 in fact ;;");
  [%expect
    {|
    let f = let rec fact = fun n -> if n then (n * fact (n - 1)) else 1 in fact;;
    |}]
;;
