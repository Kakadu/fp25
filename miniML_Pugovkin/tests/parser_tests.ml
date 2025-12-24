open Lambda_lib

(* Утилиты печати через deriving-show *)
let pp_expr s =
  try Parser.expr_of_string s |> Ast.show_expr |> print_endline with
  | Parser.Error msg -> Printf.printf "Error: %s\n" msg
;;

let pp_top s =
  try Parser.toplevel_of_string s |> Ast.show_toplevel |> print_endline with
  | Parser.Error msg -> Printf.printf "Error: %s\n" msg
;;

let pp_prog s =
  try Parser.program_of_string s |> Ast.show_program |> print_endline with
  | Parser.Error msg -> Printf.printf "Error: %s\n" msg
;;

let%expect_test "literals & vars" =
  pp_expr "42";
  pp_expr "true";
  pp_expr "false";
  pp_expr "()";
  pp_expr "x";
  [%expect
    {|
    (Const (Int 42))
    (Const (Bool true))
    (Const (Bool false))
    (Const (Unit ()))
    (Var "x")
  |}]
;;

let%expect_test "unary operators" =
  pp_expr "-x";
  pp_expr "+x";
  pp_expr "not x";
  [%expect
    {|
    (Unop (UMinus, (Var "x")))
    (Unop (UPlus, (Var "x")))
    (Unop (Not, (Var "x")))
  |}]
;;

let%expect_test "application & precedence with unary" =
  pp_expr "f x y";
  pp_expr "f (g 1) 2";
  pp_expr "-f x";
  [%expect
    {|
    (App ((App ((Var "f"), (Var "x"))), (Var "y")))
    (App ((App ((Var "f"), (App ((Var "g"), (Const (Int 1)))))), (Const (Int 2))
       ))
    (Unop (UMinus, (App ((Var "f"), (Var "x")))))
  |}]
;;

let%expect_test "arithmetic binops" =
  pp_expr "1 + 2 * 3";
  pp_expr "(1 + 2) * 3";
  pp_expr "4 - 5 - 6";
  pp_expr "8 / 2 / 2";
  [%expect
    {|
    (BinopArithmetic (Add, (Const (Int 1)),
       (BinopArithmetic (Mul, (Const (Int 2)), (Const (Int 3))))))
    (BinopArithmetic (Mul,
       (BinopArithmetic (Add, (Const (Int 1)), (Const (Int 2)))), (Const (Int 3))
       ))
    (BinopArithmetic (Sub,
       (BinopArithmetic (Sub, (Const (Int 4)), (Const (Int 5)))), (Const (Int 6))
       ))
    (BinopArithmetic (Div,
       (BinopArithmetic (Div, (Const (Int 8)), (Const (Int 2)))), (Const (Int 2))
       ))
  |}]
;;

let%expect_test "comparisons" =
  pp_expr "a = b";
  pp_expr "a <> b";
  pp_expr "a < b";
  pp_expr "a <= b";
  pp_expr "a > b";
  pp_expr "a >= b";
  [%expect
    {|
    (BinopComp (Eq, (Var "a"), (Var "b")))
    (BinopComp (Neq, (Var "a"), (Var "b")))
    (BinopComp (Lt, (Var "a"), (Var "b")))
    (BinopComp (Le, (Var "a"), (Var "b")))
    (BinopComp (Gt, (Var "a"), (Var "b")))
    (BinopComp (Ge, (Var "a"), (Var "b")))
  |}]
;;

let%expect_test "boolean operators" =
  pp_expr "true && false";
  pp_expr "true || false && true";
  [%expect
    {|
    (BinopBool (And, (Const (Bool true)), (Const (Bool false))))
    (BinopBool (Or, (Const (Bool true)),
       (BinopBool (And, (Const (Bool false)), (Const (Bool true))))))
  |}]
;;

let%expect_test "if-then[-else]" =
  pp_expr "if x then y else z";
  pp_expr "if t then u";
  [%expect
    {|
    (If ((Var "x"), (Var "y"), (Some (Var "z"))))
    (If ((Var "t"), (Var "u"), None))
  |}]
;;

let%expect_test "lambdas and let-desugaring" =
  pp_expr "fun x -> x";
  pp_expr "fun x y -> x y";
  pp_expr "let rec f x y = x + y in f 1 2";
  [%expect
    {|
    (Lam ("x", (Var "x")))
    (Lam ("x", (Lam ("y", (App ((Var "x"), (Var "y")))))))
    (Let (Rec, "f",
       (Lam ("x", (Lam ("y", (BinopArithmetic (Add, (Var "x"), (Var "y"))))))),
       (App ((App ((Var "f"), (Const (Int 1)))), (Const (Int 2))))))
  |}]
;;

let%expect_test "let-in (nonrec/rec)" =
  pp_expr "let x = 1 in x";
  pp_expr "let rec f x = x in f 1";
  [%expect
    {|
    (Let (Nonrec, "x", (Const (Int 1)), (Var "x")))
    (Let (Rec, "f", (Lam ("x", (Var "x"))), (App ((Var "f"), (Const (Int 1))))))
  |}]
;;

let%expect_test "comments (nested)" =
  pp_expr "(* nested *) 1 (* a (* b *) c *) + 2";
  [%expect {|
    (BinopArithmetic (Add, (Const (Int 1)), (Const (Int 2))))
  |}]
;;

let%expect_test "toplevel items" =
  pp_top "x + 1";
  pp_top "let x = 1";
  pp_top "let rec f x = x";
  [%expect
    {|
    (TExpr (BinopArithmetic (Add, (Var "x"), (Const (Int 1)))))
    (TLet (Nonrec, "x", (Const (Int 1))))
    (TLet (Rec, "f", (Lam ("x", (Var "x")))))
  |}]
;;

let%expect_test "program" =
  pp_prog "let x = 1;; x + 2";
  [%expect
    {|
    [(TLet (Nonrec, "x", (Const (Int 1))));
      (TExpr (BinopArithmetic (Add, (Var "x"), (Const (Int 2)))))]
  |}]
;;

let%expect_test "fix" =
  pp_expr "fix (fun f -> fun x -> f x)";
  [%expect {| (Fix (Lam ("f", (Lam ("x", (App ((Var "f"), (Var "x")))))))) |}]
;;

let%expect_test "parse errors" =
  pp_expr "let";
  pp_top "let rec";
  pp_prog "let x =";
  [%expect
    {|
    Error: : no more choices
    Error: : not enough input
    Error: : end_of_input
  |}]
;;
