(** Copyright 2021-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Filichkin_lib.Parser
open Filichkin_lib.Print

let%expect_test "parse number with different spacing" =
  print_string (print_ast (parser "42" |> Result.get_ok));
  [%expect {| Int 42 |}];
  print_string (print_ast (parser "  42  " |> Result.get_ok));
  [%expect {| Int 42 |}];
  print_string (print_ast (parser " -42" |> Result.get_ok));
  [%expect {| UnOp (Neg, Int 42) |}];
  print_string (print_ast (parser "  0  " |> Result.get_ok));
  [%expect {| Int 0 |}]
;;

let%expect_test "parse var with different spacing" =
  print_string (print_ast (parser "x" |> Result.get_ok));
  [%expect {| Var "x" |}];
  print_string (print_ast (parser "  x  " |> Result.get_ok));
  [%expect {| Var "x" |}];
  print_string (print_ast (parser "_val" |> Result.get_ok));
  [%expect {| Var "_val" |}];
  print_string (print_ast (parser "  _val  " |> Result.get_ok));
  [%expect {| Var "_val" |}];
  print_string (print_ast (parser "myVar" |> Result.get_ok));
  [%expect {| Var "myVar" |}];
  print_string (print_ast (parser "  myVar  " |> Result.get_ok));
  [%expect {| Var "myVar" |}]
;;

let%expect_test "parse binop with various spacing" =
  print_string (print_ast (parser "1+1" |> Result.get_ok));
  [%expect {| BinOp (Plus, Int 1, Int 1) |}];
  print_string (print_ast (parser "1 + 1" |> Result.get_ok));
  [%expect {| BinOp (Plus, Int 1, Int 1) |}];
  print_string (print_ast (parser "  1  +  1  " |> Result.get_ok));
  [%expect {| BinOp (Plus, Int 1, Int 1) |}];
  print_string (print_ast (parser "1 - 1" |> Result.get_ok));
  [%expect {| BinOp (Minus, Int 1, Int 1) |}];
  print_string (print_ast (parser "1  - 1" |> Result.get_ok));
  [%expect {| BinOp (Minus, Int 1, Int 1) |}];
  print_string (print_ast (parser "1*1" |> Result.get_ok));
  [%expect {| BinOp (Mult, Int 1, Int 1) |}];
  print_string (print_ast (parser "1 * 1" |> Result.get_ok));
  [%expect {| BinOp (Mult, Int 1, Int 1) |}];
  print_string (print_ast (parser "1/1" |> Result.get_ok));
  [%expect {| BinOp (Div, Int 1, Int 1) |}];
  print_string (print_ast (parser "1 / 1" |> Result.get_ok));
  [%expect {| BinOp (Div, Int 1, Int 1) |}];
  print_string (print_ast (parser "1+2*3" |> Result.get_ok));
  [%expect {| BinOp (Plus, Int 1, BinOp (Mult, Int 2, Int 3)) |}];
  print_string (print_ast (parser "1 + 2 * 3" |> Result.get_ok));
  [%expect {| BinOp (Plus, Int 1, BinOp (Mult, Int 2, Int 3)) |}];
  print_string (print_ast (parser "  1  +  2  *  3  " |> Result.get_ok));
  [%expect {| BinOp (Plus, Int 1, BinOp (Mult, Int 2, Int 3)) |}];
  print_string (print_ast (parser "(1+2)*3" |> Result.get_ok));
  [%expect {| BinOp (Mult, BinOp (Plus, Int 1, Int 2), Int 3) |}];
  print_string (print_ast (parser "(1 + 2) * 3" |> Result.get_ok));
  [%expect {| BinOp (Mult, BinOp (Plus, Int 1, Int 2), Int 3) |}];
  print_string (print_ast (parser "  (  1  +  2  )  *  3  " |> Result.get_ok));
  [%expect {| BinOp (Mult, BinOp (Plus, Int 1, Int 2), Int 3) |}];
  print_string (print_ast (parser "a+b*c -d/e" |> Result.get_ok));
  [%expect
    {| BinOp (Minus, BinOp (Plus, Var "a", BinOp (Mult, Var "b", Var "c")), BinOp (Div, Var "d", Var "e")) |}];
  print_string (print_ast (parser "a + b * c  - d / e" |> Result.get_ok));
  [%expect
    {| BinOp (Minus, BinOp (Plus, Var "a", BinOp (Mult, Var "b", Var "c")), BinOp (Div, Var "d", Var "e")) |}];
  print_string (print_ast (parser "x=5" |> Result.get_ok));
  [%expect {| BinOp (Equal, Var "x", Int 5) |}];
  print_string (print_ast (parser "x = 5" |> Result.get_ok));
  [%expect {| BinOp (Equal, Var "x", Int 5) |}];
  print_string (print_ast (parser "x>y" |> Result.get_ok));
  [%expect {| BinOp (More, Var "x", Var "y") |}];
  print_string (print_ast (parser "x > y" |> Result.get_ok));
  [%expect {| BinOp (More, Var "x", Var "y") |}];
  print_string (print_ast (parser "x>=y" |> Result.get_ok));
  [%expect {| BinOp (EMore, Var "x", Var "y") |}];
  print_string (print_ast (parser "x >= y" |> Result.get_ok));
  [%expect {| BinOp (EMore, Var "x", Var "y") |}];
  print_string (print_ast (parser "x<y+1" |> Result.get_ok));
  [%expect {| BinOp (Less, Var "x", BinOp (Plus, Var "y", Int 1)) |}]
;;

let%expect_test "parse if with various spacing" =
  print_string (print_ast (parser "if x=5 then 1 else 0" |> Result.get_ok));
  [%expect {| If (BinOp (Equal, Var "x", Int 5), Int 1, Int 0) |}];
  print_string (print_ast (parser "if x = 5 then 1 else 0" |> Result.get_ok));
  [%expect {| If (BinOp (Equal, Var "x", Int 5), Int 1, Int 0) |}];
  print_string (print_ast (parser "  if  x  =  5  then  1  else  0  " |> Result.get_ok));
  [%expect {| If (BinOp (Equal, Var "x", Int 5), Int 1, Int 0) |}];
  print_string (print_ast (parser "if a+b>c then x else y" |> Result.get_ok));
  [%expect
    {| If (BinOp (More, BinOp (Plus, Var "a", Var "b"), Var "c"), Var "x", Var "y") |}];
  print_string (print_ast (parser "if a + b > c then x else y" |> Result.get_ok));
  [%expect
    {| If (BinOp (More, BinOp (Plus, Var "a", Var "b"), Var "c"), Var "x", Var "y") |}]
;;

let%expect_test "parse let with various spacing" =
  print_string (print_ast (parser "let x=5" |> Result.get_ok));
  [%expect {| Let (NonRec, "PVar x", Int 5, None) |}];
  print_string (print_ast (parser "let x = 5" |> Result.get_ok));
  [%expect {| Let (NonRec, "PVar x", Int 5, None) |}];
  print_string (print_ast (parser "  let  x  =  5  " |> Result.get_ok));
  [%expect {| Let (NonRec, "PVar x", Int 5, None) |}];
  print_string (print_ast (parser "let x=5 in x+1" |> Result.get_ok));
  [%expect {| Let (NonRec, "PVar x", Int 5, Some BinOp (Plus, Var "x", Int 1)) |}];
  print_string (print_ast (parser "let x = 5 in x + 1" |> Result.get_ok));
  [%expect {| Let (NonRec, "PVar x", Int 5, Some BinOp (Plus, Var "x", Int 1)) |}];
  print_string (print_ast (parser "  let  x  =  5  in  x  +  1  " |> Result.get_ok));
  [%expect {| Let (NonRec, "PVar x", Int 5, Some BinOp (Plus, Var "x", Int 1)) |}];
  print_string (print_ast (parser "let rec f x = x+1" |> Result.get_ok));
  [%expect {| Let (Rec, "PVar f", Abs ("x", BinOp (Plus, Var "x", Int 1)), None) |}];
  print_string (print_ast (parser "let rec f x = x + 1" |> Result.get_ok));
  [%expect {| Let (Rec, "PVar f", Abs ("x", BinOp (Plus, Var "x", Int 1)), None) |}];
  print_string (print_ast (parser "  let  rec  f  x  =  x  +  1  " |> Result.get_ok));
  [%expect {| Let (Rec, "PVar f", Abs ("x", BinOp (Plus, Var "x", Int 1)), None) |}];
  print_string
    (print_ast (parser "let x=2+3*4 in if x>10 then 1 else 0" |> Result.get_ok));
  [%expect
    {| Let (NonRec, "PVar x", BinOp (Plus, Int 2, BinOp (Mult, Int 3, Int 4)), Some If (BinOp (More, Var "x", Int 10), Int 1, Int 0)) |}]
;;

let%expect_test "parse abs with various spacing" =
  print_string (print_ast (parser "fun x -> x+1" |> Result.get_ok));
  [%expect {| Abs ("x", BinOp (Plus, Var "x", Int 1)) |}];
  print_string (print_ast (parser "fun x  -> x + 1" |> Result.get_ok));
  [%expect {| Abs ("x", BinOp (Plus, Var "x", Int 1)) |}];
  print_string (print_ast (parser "  fun  x   ->  x  +  1  " |> Result.get_ok));
  [%expect {| Abs ("x", BinOp (Plus, Var "x", Int 1)) |}];
  print_string (print_ast (parser "fun x y -> x+y" |> Result.get_ok));
  [%expect {| Abs ("x", Abs ("y", BinOp (Plus, Var "x", Var "y"))) |}];
  print_string (print_ast (parser "fun x y  -> x + y" |> Result.get_ok));
  [%expect {| Abs ("x", Abs ("y", BinOp (Plus, Var "x", Var "y"))) |}];
  print_string (print_ast (parser "fun x  -> if x>0 then x else  -x" |> Result.get_ok));
  [%expect
    {| Abs ("x", If (BinOp (More, Var "x", Int 0), Var "x", UnOp (Neg, Var "x"))) |}]
;;

let%expect_test "parse app with various spacing" =
  print_string (print_ast (parser "f x" |> Result.get_ok));
  [%expect {| App (Var "f", Var "x") |}];
  print_string (print_ast (parser "f  x" |> Result.get_ok));
  [%expect {| App (Var "f", Var "x") |}];
  print_string (print_ast (parser "f(x)" |> Result.get_ok));
  [%expect {| App (Var "f", Var "x") |}];
  print_string (print_ast (parser "f (x)" |> Result.get_ok));
  [%expect {| App (Var "f", Var "x") |}];
  print_string (print_ast (parser "f x y" |> Result.get_ok));
  [%expect {| App (App (Var "f", Var "x"), Var "y") |}];
  print_string (print_ast (parser "f(x y)" |> Result.get_ok));
  [%expect {| App (Var "f", App (Var "x", Var "y")) |}];
  print_string (print_ast (parser "f (x) (y)" |> Result.get_ok));
  [%expect {| App (App (Var "f", Var "x"), Var "y") |}];
  print_string (print_ast (parser "(fun x -> x+1) 5" |> Result.get_ok));
  [%expect {| App (Abs ("x", BinOp (Plus, Var "x", Int 1)), Int 5) |}];
  print_string (print_ast (parser "(fun x  -> x + 1) 5" |> Result.get_ok));
  [%expect {| App (Abs ("x", BinOp (Plus, Var "x", Int 1)), Int 5) |}];
  print_string (print_ast (parser "let f=fun x -> x+1 in f 5" |> Result.get_ok));
  [%expect
    {| Let (NonRec, "PVar f", Abs ("x", BinOp (Plus, Var "x", Int 1)), Some App (Var "f", Int 5)) |}]
;;

let%expect_test "parse complex expressions with various spacing" =
  print_string (print_ast (parser "if let x=5 in x>0 then 1 else 0" |> Result.get_ok));
  [%expect
    {| If (Let (NonRec, "PVar x", Int 5, Some BinOp (More, Var "x", Int 0)), Int 1, Int 0) |}];
  print_string
    (print_ast
       (parser "let f=fun x -> if x>0 then x else 0 in f 5+f( -3)" |> Result.get_ok));
  [%expect
    {| Let (NonRec, "PVar f", Abs ("x", If (BinOp (More, Var "x", Int 0), Var "x", Int 0)), Some BinOp (Plus, App (Var "f", Int 5), App (Var "f", UnOp (Neg, Int 3)))) |}];
  print_string
    (print_ast
       (parser "let rec fact n = if n<=1 then 1 else n*fact(n -1) in fact 5"
        |> Result.get_ok));
  [%expect
    {| Let (Rec, "PVar fact", Abs ("n", If (BinOp (ELess, Var "n", Int 1), Int 1, BinOp (Mult, Var "n", App (Var "fact", BinOp (Minus, Var "n", Int 1))))), Some App (Var "fact", Int 5)) |}];
  print_string
    (print_ast
       (parser
          "  let  rec  fact  n  =  if  n  <=  1  then  1  else  n  *  fact  (  n   -  1  \
           )  in  fact  5  "
        |> Result.get_ok));
  [%expect
    {| Let (Rec, "PVar fact", Abs ("n", If (BinOp (ELess, Var "n", Int 1), Int 1, BinOp (Mult, Var "n", App (Var "fact", BinOp (Minus, Var "n", Int 1))))), Some App (Var "fact", Int 5)) |}]
;;

let%expect_test "parse nested expressions" =
  print_string (print_ast (parser "((((5))))" |> Result.get_ok));
  [%expect {| Int 5 |}];
  print_string (print_ast (parser "(  (  (  (  5  )  )  )  )" |> Result.get_ok));
  [%expect {| Int 5 |}];
  print_string (print_ast (parser "1+(2*(3 -4)/5)" |> Result.get_ok));
  [%expect
    {| BinOp (Plus, Int 1, BinOp (Div, BinOp (Mult, Int 2, BinOp (Minus, Int 3, Int 4)), Int 5)) |}];
  print_string (print_ast (parser "1 + (2 * (3  - 4) / 5)" |> Result.get_ok));
  [%expect
    {| BinOp (Plus, Int 1, BinOp (Div, BinOp (Mult, Int 2, BinOp (Minus, Int 3, Int 4)), Int 5)) |}];
  print_string
    (print_ast (parser "if (if a then b else c) then d else e" |> Result.get_ok));
  [%expect {| If (If (Var "a", Var "b", Var "c"), Var "d", Var "e") |}];
  print_string (print_ast (parser "let x = let y = 2 in y*3 in x+1" |> Result.get_ok));
  [%expect
    {| Let (NonRec, "PVar x", Let (NonRec, "PVar y", Int 2, Some BinOp (Mult, Var "y", Int 3)), Some BinOp (Plus, Var "x", Int 1)) |}]
;;

let%expect_test "parse with extreme spacing" =
  print_string (print_ast (parser "1+2*3 -4/5" |> Result.get_ok));
  [%expect
    {| BinOp (Minus, BinOp (Plus, Int 1, BinOp (Mult, Int 2, Int 3)), BinOp (Div, Int 4, Int 5)) |}];
  print_string (print_ast (parser "  1  +  2  *  3   -  4  /  5  " |> Result.get_ok));
  [%expect
    {| BinOp (Minus, BinOp (Plus, Int 1, BinOp (Mult, Int 2, Int 3)), BinOp (Div, Int 4, Int 5)) |}];
  print_string (print_ast (parser "if  x>0  then  let y=5 in y else 0" |> Result.get_ok));
  [%expect
    {| If (BinOp (More, Var "x", Int 0), Let (NonRec, "PVar y", Int 5, Some Var "y"), Int 0) |}];
  print_string (print_ast (parser "if x>0 then let y=5 in y else 0" |> Result.get_ok));
  [%expect
    {| If (BinOp (More, Var "x", Int 0), Let (NonRec, "PVar y", Int 5, Some Var "y"), Int 0) |}]
;;

let%expect_test "parsing of basic structures" =
  print_string (print_ast (parser "10 - 4 / 2" |> Result.get_ok));
  [%expect {| BinOp (Minus, Int 10, BinOp (Div, Int 4, Int 2)) |}];
  print_string (print_ast (parser "true && false" |> Result.get_ok));
  [%expect {| BinOp (And, Bool (true), Bool (false)) |}];
  print_string (print_ast (parser "true || false" |> Result.get_ok));
  [%expect {| BinOp (Or, Bool (true), Bool (false)) |}];
  print_string (print_ast (parser "not true" |> Result.get_ok));
  [%expect {| UnOp (Not, Bool (true)) |}];
  print_string (print_ast (parser "5 > 3" |> Result.get_ok));
  [%expect {| BinOp (More, Int 5, Int 3) |}];
  print_string (print_ast (parser "5 = 5" |> Result.get_ok));
  [%expect {| BinOp (Equal, Int 5, Int 5) |}];
  print_string (print_ast (parser "5 <> 3" |> Result.get_ok));
  [%expect {| BinOp (NotEqual, Int 5, Int 3) |}];
  print_string (print_ast (parser "5 >= 5" |> Result.get_ok));
  [%expect {| BinOp (EMore, Int 5, Int 5) |}];
  print_string (print_ast (parser "3 <= 4" |> Result.get_ok));
  [%expect {| BinOp (ELess, Int 3, Int 4) |}];
  print_string (print_ast (parser "if true then 1 else 0" |> Result.get_ok));
  [%expect {| If (Bool (true), Int 1, Int 0) |}];
  print_string (print_ast (parser "if 5 > 3 then 10 else 20" |> Result.get_ok));
  [%expect {| If (BinOp (More, Int 5, Int 3), Int 10, Int 20) |}];
  print_string
    (print_ast (parser "if false then 1 else if true then 2 else 3" |> Result.get_ok));
  [%expect {| If (Bool (false), Int 1, If (Bool (true), Int 2, Int 3)) |}];
  print_string (print_ast (parser "(fun x -> x + 1) 5" |> Result.get_ok));
  [%expect {| App (Abs ("x", BinOp (Plus, Var "x", Int 1)), Int 5) |}];
  print_string (print_ast (parser "(fun x y -> x + y) 2 3" |> Result.get_ok));
  [%expect
    {| App (App (Abs ("x", Abs ("y", BinOp (Plus, Var "x", Var "y"))), Int 2), Int 3) |}];
  print_string (print_ast (parser "let x = 5 in x + 3" |> Result.get_ok));
  [%expect {| Let (NonRec, "PVar x", Int 5, Some BinOp (Plus, Var "x", Int 3)) |}];
  print_string (print_ast (parser "let x = 1 in let y = 2 in x + y" |> Result.get_ok));
  [%expect
    {| Let (NonRec, "PVar x", Int 1, Some Let (NonRec, "PVar y", Int 2, Some BinOp (Plus, Var "x", Var "y"))) |}];
  print_string
    (print_ast
       (parser "let rec fact = fun n -> if n = 0 then 1 else n * fact (n - 1) in fact 5"
        |> Result.get_ok));
  [%expect
    {| Let (Rec, "PVar fact", Abs ("n", If (BinOp (Equal, Var "n", Int 0), Int 1, BinOp (Mult, Var "n", App (Var "fact", BinOp (Minus, Var "n", Int 1))))), Some App (Var "fact", Int 5)) |}];
  print_string (print_ast (parser "(1, 2, 3)" |> Result.get_ok));
  [%expect {| Tuple [Int 1; Int 2; Int 3] |}];
  print_string (print_ast (parser "let (x, y) = (1, 2) in x + y" |> Result.get_ok));
  [%expect
    {| Let (NonRec, "PTuple [PVar x; PVar y]", Tuple [Int 1; Int 2], Some BinOp (Plus, Var "x", Var "y")) |}];
  print_string
    (print_ast
       (parser "let t = (1, 2, 3) in let (a, b, c) = t in a + b + c" |> Result.get_ok));
  [%expect
    {| Let (NonRec, "PVar t", Tuple [Int 1; Int 2; Int 3], Some Let (NonRec, "PTuple [PVar a; PVar b; PVar c]", Var "t", Some BinOp (Plus, BinOp (Plus, Var "a", Var "b"), Var "c"))) |}];
  print_string
    (print_ast (parser "let add = fun x y -> x + y in add 3 4" |> Result.get_ok));
  [%expect
    {| Let (NonRec, "PVar add", Abs ("x", Abs ("y", BinOp (Plus, Var "x", Var "y"))), Some App (App (Var "add", Int 3), Int 4)) |}];
  print_string
    (print_ast
       (parser "let twice = fun f x -> f (f x) in let inc = fun x -> x + 1 in twice inc 5"
        |> Result.get_ok));
  [%expect
    {| Let (NonRec, "PVar twice", Abs ("f", Abs ("x", App (Var "f", App (Var "f", Var "x")))), Some Let (NonRec, "PVar inc", Abs ("x", BinOp (Plus, Var "x", Int 1)), Some App (App (Var "twice", Var "inc"), Int 5))) |}];
  print_string (print_ast (parser "let x = 5 in print_int (x * 2)" |> Result.get_ok));
  [%expect
    {| Let (NonRec, "PVar x", Int 5, Some App (Var "print_int", BinOp (Mult, Var "x", Int 2))) |}];
  print_string
    (print_ast
       (parser
          "let compose = fun f g x -> f (g x) in let inc = fun x -> x + 1 in let square \
           = fun x -> x * x in compose inc square 3"
        |> Result.get_ok));
  [%expect
    {| Let (NonRec, "PVar compose", Abs ("f", Abs ("g", Abs ("x", App (Var "f", App (Var "g", Var "x"))))), Some Let (NonRec, "PVar inc", Abs ("x", BinOp (Plus, Var "x", Int 1)), Some Let (NonRec, "PVar square", Abs ("x", BinOp (Mult, Var "x", Var "x")), Some App (App (App (Var "compose", Var "inc"), Var "square"), Int 3)))) |}];
  print_string
    (print_ast (parser "let swap = fun (x, y) -> (y, x) in swap (1, 2)" |> Result.get_ok));
  [%expect
    {| Let (NonRec, "PVar swap", Abs ("(x, y)", Tuple [Var "y"; Var "x"]), Some App (Var "swap", Tuple [Int 1; Int 2])) |}]
;;
