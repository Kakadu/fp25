open DMotuzov_lib.Ast
open DMotuzov_lib.Parser

let run input =
  match parse input with
  | Ok structure -> Stdlib.Format.printf "%s\n" (show_structure structure)
  | Error err -> Stdlib.Format.printf "%s\n" err
;;

(* --- BASIC CONSTANTS & IDENTIFIERS --- *)

let%expect_test "parse_simple_constant" =
  run "let x = 42 ;;";
  [%expect {| [(Top_let ("x", (Expr_const (Const_int 42))))] |}]
;;

let%expect_test "parse_simple_identifier" =
  run "let x = y ;;";
  [%expect {| [(Top_let ("x", (Expr_var "y")))] |}]
;;

(* --- ARITHMETIC --- *)

let%expect_test "parse_add" =
  run "let x = 1 + 2 ;;";
  [%expect
    {|
    [(Top_let ("x",
        (Expr_binary_op (Plus, (Expr_const (Const_int 1)),
           (Expr_const (Const_int 2))))
        ))
      ] |}]
;;

let%expect_test "parse_mul_precedence" =
  run "let x = 1 + 2 * 3 ;;";
  [%expect
    {|
    [(Top_let ("x",
        (Expr_binary_op (Plus, (Expr_const (Const_int 1)),
           (Expr_binary_op (Mul, (Expr_const (Const_int 2)),
              (Expr_const (Const_int 3))))
           ))
        ))
      ] |}]
;;

let%expect_test "parse_nested_arithmetic" =
  run "let x = (1 + 2) * (3 - 4) ;;";
  [%expect
    {|
    [(Top_let ("x",
        (Expr_binary_op (Mul,
           (Expr_binary_op (Plus, (Expr_const (Const_int 1)),
              (Expr_const (Const_int 2)))),
           (Expr_binary_op (Sub, (Expr_const (Const_int 3)),
              (Expr_const (Const_int 4))))
           ))
        ))
      ] |}]
;;

(* --- IF-THEN-ELSE --- *)

let%expect_test "parse_if_simple" =
  run "let x = if 1 then 2 else 3 ;;";
  [%expect
    {|
    [(Top_let ("x",
        (Expr_conditional ((Expr_const (Const_int 1)),
           (Expr_const (Const_int 2)), (Expr_const (Const_int 3))))
        ))
      ] |}]
;;

let%expect_test "parse_if_in_binop" =
  run "let x = 2 + if 0 then 10 else 20 ;;";
  [%expect
    {|
    [(Top_let ("x",
        (Expr_binary_op (Plus, (Expr_const (Const_int 2)),
           (Expr_conditional ((Expr_const (Const_int 0)),
              (Expr_const (Const_int 10)), (Expr_const (Const_int 20))))
           ))
        ))
      ] |}]
;;

let%expect_test "parse_nested_if" =
  run "let x = if 1 then if 0 then 2 else 3 else 4 ;;";
  [%expect
    {|
    [(Top_let ("x",
        (Expr_conditional ((Expr_const (Const_int 1)),
           (Expr_conditional ((Expr_const (Const_int 0)),
              (Expr_const (Const_int 2)), (Expr_const (Const_int 3)))),
           (Expr_const (Const_int 4))))
        ))
      ] |}]
;;

(* --- FUNCTIONS --- *)

let%expect_test "parse_lambda_one_arg" =
  run "let f = fun x -> x + 1 ;;";
  [%expect
    {|
    [(Top_let ("f",
        (Expr_fun ("x",
           (Expr_binary_op (Plus, (Expr_var "x"), (Expr_const (Const_int 1))))))
        ))
      ] |}]
;;

let%expect_test "parse_lambda_two_args_sugar" =
  run "let f = fun x y -> x + y ;;";
  [%expect {|
    [(Top_let ("f",
        (Expr_fun ("x",
           (Expr_fun ("y",
              (Expr_binary_op (Plus, (Expr_var "x"), (Expr_var "y")))))
           ))
        ))
      ] |}]
;;

let%expect_test "parse_lambda_nested" =
  run "let f = fun x -> fun y -> x * y ;;";
  [%expect
    {|
    [(Top_let ("f",
        (Expr_fun ("x",
           (Expr_fun ("y", (Expr_binary_op (Mul, (Expr_var "x"), (Expr_var "y")))
              ))
           ))
        ))
      ] |}]
;;

let%expect_test "parse_application" =
  run "let x = f 10 20 ;;";
  [%expect
    {|
    [(Top_let ("x",
        (Expr_ap ((Expr_var "f"),
           [(Expr_const (Const_int 10)); (Expr_const (Const_int 20))]))
        ))
      ] |}]
;;

let%expect_test "parse_application_parens" =
  run "let x = (fun x y -> x + y) 3 4 ;;";
  [%expect {|
    [(Top_let ("x",
        (Expr_ap (
           (Expr_fun ("x",
              (Expr_fun ("y",
                 (Expr_binary_op (Plus, (Expr_var "x"), (Expr_var "y")))))
              )),
           [(Expr_const (Const_int 3)); (Expr_const (Const_int 4))]))
        ))
      ] |}]
;;

(* --- LET & LET REC --- *)

let%expect_test "parse_let_in" =
  run "let x = let y = 10 in y + 2 ;;";
  [%expect {|
    [(Top_let ("x",
        (Expr_let_in ("y", (Expr_const (Const_int 10)),
           (Expr_binary_op (Plus, (Expr_var "y"), (Expr_const (Const_int 2))))))
        ))
      ] |}]
;;

let%expect_test "parse_let_rec" =
  run "let f = let rec fact = fun n -> if n then n * fact (n - 1) else 1 in fact ;;";
  [%expect {|
    [(Top_let ("f",
        (Expr_let_rec_in ("fact",
           (Expr_fun ("n",
              (Expr_conditional ((Expr_var "n"),
                 (Expr_binary_op (Mul, (Expr_var "n"),
                    (Expr_ap ((Expr_var "fact"),
                       [(Expr_binary_op (Sub, (Expr_var "n"),
                           (Expr_const (Const_int 1))))
                         ]
                       ))
                    )),
                 (Expr_const (Const_int 1))))
              )),
           (Expr_var "fact")))
        ))
      ] |}]
;;

(* --- FIX --- *)

let%expect_test "parse_fix" =
  run "let f = fix (fun f -> fun x -> if x then x + (f (x - 1)) else 0) ;;";
  [%expect
    {|
    [(Top_let ("f",
        (Expr_fix
           (Expr_fun ("f",
              (Expr_fun ("x",
                 (Expr_conditional ((Expr_var "x"),
                    (Expr_binary_op (Plus, (Expr_var "x"),
                       (Expr_ap ((Expr_var "f"),
                          [(Expr_binary_op (Sub, (Expr_var "x"),
                              (Expr_const (Const_int 1))))
                            ]
                          ))
                       )),
                    (Expr_const (Const_int 0))))
                 ))
              )))
        ))
      ] |}]
;;

(* --- MULTIPLE BINDINGS --- *)

let%expect_test "parse_two_bindings" =
  run "let x = 1 ;; let y = 2 ;;";
  [%expect
    {|
    [(Top_let ("x", (Expr_const (Const_int 1))));
      (Top_let ("y", (Expr_const (Const_int 2))))] |}]
;;

let%expect_test "parse_three_bindings_no_spaces" =
  run "let x=1;;let y=2;;let z=3;;";
  [%expect
    {|
    [(Top_let ("x", (Expr_const (Const_int 1))));
      (Top_let ("y", (Expr_const (Const_int 2))));
      (Top_let ("z", (Expr_const (Const_int 3))))] |}]
;;

(* --- COMPLEX PROGRAMS (FACTORIAL, FIBONACCI) --- *)

let%expect_test "parse_fact" =
  run "let rec fact = fun n -> if n then n * fact (n - 1) else 1 ;;";
  [%expect
    {|
    [(Top_let_rec ("fact",
        (Expr_fun ("n",
           (Expr_conditional ((Expr_var "n"),
              (Expr_binary_op (Mul, (Expr_var "n"),
                 (Expr_ap ((Expr_var "fact"),
                    [(Expr_binary_op (Sub, (Expr_var "n"),
                        (Expr_const (Const_int 1))))
                      ]
                    ))
                 )),
              (Expr_const (Const_int 1))))
           ))
        ))
      ] |}]
;;

let%expect_test "parse_fib" =
  run
    "let rec fib = fun n -> if n then if n - 1 then fib (n - 1) + fib (n - 2) else 1 \
     else 0 ;;";
  [%expect
    {|
    [(Top_let_rec ("fib",
        (Expr_fun ("n",
           (Expr_conditional ((Expr_var "n"),
              (Expr_conditional (
                 (Expr_binary_op (Sub, (Expr_var "n"), (Expr_const (Const_int 1))
                    )),
                 (Expr_binary_op (Plus,
                    (Expr_ap ((Expr_var "fib"),
                       [(Expr_binary_op (Sub, (Expr_var "n"),
                           (Expr_const (Const_int 1))))
                         ]
                       )),
                    (Expr_ap ((Expr_var "fib"),
                       [(Expr_binary_op (Sub, (Expr_var "n"),
                           (Expr_const (Const_int 2))))
                         ]
                       ))
                    )),
                 (Expr_const (Const_int 1)))),
              (Expr_const (Const_int 0))))
           ))
        ))
      ] |}]
;;
