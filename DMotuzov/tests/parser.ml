open DMotuzov_lib.Ast
open DMotuzov_lib.Parser

let run input =
  match parse input with
  | Ok structure -> Stdlib.Format.printf "%s\n" (show_structure structure)
  | Error err -> Stdlib.Format.printf "%s\n" err
;;

let%expect_test "parse_multiple_bindings" =
  run "let x = 1 ;; let y = 5 ;;";
  [%expect
    {|
    [(Top_let ((Id "x"), (Expr_const (Const_int 1))));
      (Top_let ((Id "y"), (Expr_const (Const_int 5))))] |}]
;;

let%expect_test "parse_bin_op" =
  run "let x = 1 + 2 * 3 + 4;;";
  [%expect
    {|
    [(Top_let ((Id "x"),
        (Expr_binary_op (Plus,
           (Expr_binary_op (Plus, (Expr_const (Const_int 1)),
              (Expr_binary_op (Mul, (Expr_const (Const_int 2)),
                 (Expr_const (Const_int 3))))
              )),
           (Expr_const (Const_int 4))))
        ))
      ] |}]
;;

let%expect_test "parse_parens" =
  run "let x = (2+4) ;;";
  [%expect
    {|
    [(Top_let ((Id "x"),
        (Expr_binary_op (Plus, (Expr_const (Const_int 2)),
           (Expr_const (Const_int 4))))
        ))
      ] |}]
;;

let%expect_test "parse_if_then_else" =
  run "let x = (2+4) + if 2 then 1 else 0 ;;";
  [%expect
    {|
    [(Top_let ((Id "x"),
        (Expr_binary_op (Plus,
           (Expr_binary_op (Plus, (Expr_const (Const_int 2)),
              (Expr_const (Const_int 4)))),
           (Expr_conditional ((Expr_const (Const_int 2)),
              (Expr_const (Const_int 1)), (Expr_const (Const_int 0))))
           ))
        ))
      ] |}]
;;
