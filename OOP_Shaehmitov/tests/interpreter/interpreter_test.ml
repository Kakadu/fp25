[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Parser
open Interpreter

let run_interpreter_test input =
  let program = parse_structure_items input in
  match program with
  | Error msg -> Printf.printf "Parse Error: %s\n" msg
  | Ok prog ->
    (match Interpreter.run_program 100000 prog with
     | Ok _ -> ()
     | Error err -> Printf.printf "Runtime Error: %s\n" (Interpreter.show_error err))
;;

let%expect_test "interpreter test print int simple" =
  run_interpreter_test "let () = print_int 5";
  [%expect {| 5 |}]
;;

let%expect_test "interpreter test print int expression" =
  run_interpreter_test "let () = print_int (2 + 3 * 4)";
  [%expect {| 14 |}]
;;

let%expect_test "interpreter test let binding and print" =
  run_interpreter_test "let () = let x = 10 in print_int (x * 2)";
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
  [%expect {| Runtime Error: Error: Type error - if |}]
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
  [%expect {| Runtime Error: Error: Type error - if |}]
;;

let%expect_test "lambda binding" =
  run_interpreter_test
    "let add = fun x y z -> x + y + z ;; let () = print_int (add 3 4 5)";
  [%expect {|12|}]
;;

let%expect_test "factorial with fix" =
  run_interpreter_test
    "\n\
    \    let rec fix f x = f (fix f) x ;; \n\
    \    let fact = fix (fun slf n -> if n = 0 then 1 else n * slf (n - 1)) ;;\n\
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
  [%expect {| Runtime Error: Error: Type error - rec def |}]
;;

let%expect_test "unit function test" =
  run_interpreter_test "let rec () = print_int 42";
  [%expect {| Runtime Error: Error: Type error - rec def |}]
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
  [%expect {| Runtime Error: Error: Type error - bin op mismatch |}]
;;

let%expect_test "print_endl test" =
  run_interpreter_test "let () = print_endl ()";
  [%expect {| |}]
;;

let%expect_test "unit unop" =
  run_interpreter_test "let () = print_int (-())";
  [%expect {| Runtime Error: Error: Type error - neg |}]
;;

let%expect_test "unit condition" =
  run_interpreter_test "let () = if () then 1 else 0";
  [%expect {| Runtime Error: Error: Type error - if |}]
;;

let%expect_test "any pattern in value binding" =
  run_interpreter_test "let _ = print_int 42";
  [%expect {| 42 |}]
;;

let%expect_test "applying argument to function with no parameters" =
  run_interpreter_test "let f = 42 ;; let () = print_int (f 5)";
  [%expect {| Runtime Error: Error: Type error - app non-func |}]
;;

let%expect_test "equality operator test" =
  run_interpreter_test "let () = print_int (if 5 = 5 then 1 else 0)";
  [%expect {| 1 |}]
;;

let%expect_test "inequality operator test" =
  run_interpreter_test "let () = print_int (if 5 <> 3 then 1 else 0)";
  [%expect {| 1 |}]
;;

let%expect_test "less than operator test" =
  run_interpreter_test "let () = print_int (if 3 < 5 then 1 else 0)";
  [%expect {| 1 |}]
;;

let%expect_test "greater than or equal operator test" =
  run_interpreter_test "let () = print_int (if 5 >= 5 then 1 else 0)";
  [%expect {| 1 |}]
;;

let%expect_test "subtraction and multiplication" =
  run_interpreter_test "let () = print_int (10 - 2 * 3)";
  [%expect {| 4 |}]
;;

let%expect_test "division by zero" =
  run_interpreter_test "let () = print_int (10 / 0)";
  [%expect {| Runtime Error: Error: Division by zero |}]
;;

let%expect_test "remaining comparison operators" =
  run_interpreter_test
    "let () = print_int (if 5 > 4 then 1 else 0);; \n\
     let () = print_int (if 4 <= 4 then 1 else 0)";
  [%expect {| 11 |}]
;;

let%expect_test "recursive let wildcard error" =
  run_interpreter_test "let rec _ = fun x -> x";
  [%expect {| Runtime Error: Error: Type error - rec def |}]
;;

let%expect_test "unit pattern mismatch in let" =
  run_interpreter_test "let () = 5";
  [%expect {| Runtime Error: Error: Type error - Pattern binding failed |}]
;;

let%expect_test "unit pattern mismatch in function application" =
  run_interpreter_test "let f () = 1 ;; let () = print_int (f 5)";
  [%expect {| Runtime Error: Error: Type error - Pattern binding failed |}]
;;

let%expect_test "wildcard argument in function" =
  run_interpreter_test "let f _ = 42 ;; let () = print_int (f 100)";
  [%expect {| 42 |}]
;;

let%expect_test "print_endl type error" =
  run_interpreter_test "let () = print_endl 5";
  [%expect {| Runtime Error: Error: Type error - p_unit |}]
;;

let%expect_test "print_int type error" =
  run_interpreter_test "let () = print_int ()";
  [%expect {| Runtime Error: Error: Type error - p_int |}]
;;

let%expect_test "let rec with non-function error" =
  run_interpreter_test "let rec f = 42";
  [%expect {| Runtime Error: Error: Type error - rec def |}]
;;

let%expect_test "applying argument to function with no parameters (fix)" =
  run_interpreter_test
    "let rec fix f x = f (fix f) x ;; let g = let f = fix (fun x -> 10) in f 1";
  [%expect {| Runtime Error: Error: Type error - app non-func |}]
;;

let%expect_test "inc" =
  run_interpreter_test
    "let inc x = \n\
    \      let add = fun x y -> x + y in\n\
    \      add x 1 ;;\n\
    \     let () = print_int (inc 41)";
  [%expect {| 42 |}]
;;

let%expect_test "class test" =
  run_interpreter_test
    "class Adder = \n\
    \  object\n\
    \    val x = 0 \n\
    \    method add y = x + y \n\
    \  end ;; \n\
    \ let a = new Adder ;; \n\
    \ let () = print_int (a#add 5)";
  [%expect {| 5 |}]
;;

let%expect_test "factorial with class" =
  run_interpreter_test
    "class Factorial = \n\
    \  object(self)\n\
    \    method fact n = \n\
    \      if n = 0 then 1 else n * self#fact (n - 1) \n\
    \  end ;; \n\
    \ let f = new Factorial ;; \n\
    \ let () = print_int (f#fact 10)";
  [%expect {| 3628800 |}]
;;

let%expect_test "class in class" =
  run_interpreter_test
    "class Point x y = \n\
    \  object\n\
    \    val x = x \n\
    \    val y = y \n\
    \    method get_x = x \n\
    \    method get_y = y \n\
    \    method move dx dy = new Point (x+dx) (y+dy) \n\
    \  end ;; \n\
    \ let p = new Point 0 0 ;; \n\
    \ let p2 = p#move 3 4 ;; \n\
    \ let () = print_int (p2#get_x + p2#get_y)";
  [%expect {| 7 |}]
;;

let%expect_test "inheritance test" =
  run_interpreter_test
    "\n\
    \    class Animal age = object\n\
    \      val age = age\n\
    \      method get_age = age\n\
    \    end;;\n\
    \    class Dog age name = object\n\
    \      inherit Animal age\n\
    \      val name = name\n\
    \      method get_name = name\n\
    \    end;;\n\
    \    let d = new Dog 5 2;;\n\
    \    let age = d#get_age;;\n\
    \    let name = d#get_name;;\n\
    \    let () = print_int (age + name)\n\
    \  ";
  [%expect {| 7 |}]
;;

let%expect_test "tuple pattern in let binding" =
  run_interpreter_test "let (x, y) = (3, 4) ;; \nlet () = print_int (x * y)";
  [%expect {| 12 |}]
;;

let%expect_test "class test" =
  run_interpreter_test
    "\n\
    \    class Parent = object method p = 1 end;;\n\
    \    class Child = object inherit Parent method c = 2 end;;\n\
    \    \n\
    \    let force_parent x = \n\
    \       let p = new Parent in \n\
    \       if true then x else p;;\n\
    \    \n\
    \    let c = new Child;;\n\
    \    \n\
    \    let res = print_int ((force_parent c)#c);;\n\
    \  ";
  [%expect {| 2 |}]
;;

let%expect_test "class test 2" =
  run_interpreter_test
    "\n\
    \    class Parent x y = object val x = x  val y = y  method get_x = x end;;\n\
    \    class Child (x, y) = object inherit Parent x y method get_y = y end;;\n\
    \    \n\
    \    let c = new Child (10, 20);;\n\
    \    let x_val = c#get_x;;\n\
    \    let y_val = c#get_y;;\n\
    \    let () = print_int (x_val + y_val);;\n\
    \         ";
  [%expect {| 30 |}]
;;

let%expect_test "field overriding test" =
  run_interpreter_test
    "\n\
    \    class Parent = object\n\
    \      val x = 10\n\
    \    method get_x = x\n\
    \    end;;\n\
    \    class Child = object\n\
    \      inherit Parent\n\
    \      val x = true\n\
    \    end;;\n\
    \    let p = new Parent;;\n\
    \    let c = new Child;;\n\
    \    let x_p = p#get_x;;\n\
    \    let x_c = c#get_x;;\n\
    \       let () = print_int (x_p + x_c);;\n\
    \  ";
  [%expect {| 20 |}]
;;

let%expect_test "self return" =
  run_interpreter_test
    "\n\
    \    class Chain = \n\
    \      object(self)\n\
    \        method get_self = self\n\
    \        method get_value = 42\n\
    \      end;;\n\
    \    let c = new Chain;;\n\
    \    let c2 = c#get_self#get_self#get_self ;;\n\
    \    let v = c2#get_value ;;\n\
    \    let () = print_int v;;\n\
    \  ";
  [%expect {| 42 |}]
;;

let%expect_test "field overriding test 2" =
  run_interpreter_test
    "\n\
    \    class Parent = object\n\
    \      val x = 10\n\
    \    method get_x = x\n\
    \    end;;\n\
    \    class Child = object\n\
    \      inherit Parent\n\
    \      val x = 5\n\
    \      method get_x1 = x\n\
    \    end;;\n\
    \     class GrandChild = object\n\
    \      inherit Child\n\
    \      val x = 15\n\
    \    end;;\n\n\
    \        let p = new Parent;;\n\
    \    let c = new Child;;\n\
    \    let gc = new GrandChild;;\n\
    \    let x_p = p#get_x;;\n\
    \    let x_c = c#get_x;;\n\
    \    let x_gc = gc#get_x1;;\n\
    \       let () = print_int (x_p + x_c + x_gc);;\n\
    \  ";
  [%expect {| 25 |}]
;;
