(** Copyright 2025, Tenyaeva Ekaterina *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

  $ ../bin/REPL.exe <<EOF
  > 122
  122

  $ ../bin/REPL.exe <<EOF
  > (08 + 122 / 2) * 3 - 4
  203

  $ ../bin/REPL.exe <<EOF
  > -4 + 8 + (+3)
  7

  $ ../bin/REPL.exe <<EOF
  > 5 <> 3
  true

  $ ../bin/REPL.exe <<EOF
  > (4 <= 8) = true
  true

  $ ../bin/REPL.exe <<EOF
  > let Some x = Some (1 : int) and None = None
  val x = 1

  $ ../bin/REPL.exe <<EOF
  > let x = false;; not x;;
  val x = false
  true

  $ ../bin/REPL.exe <<EOF
  > if 6 < 10 then 1 else (if 6 >= 0 then 2 else (if 9 > 10 then 3 else 4));;
  1

  $ ../bin/REPL.exe -max-steps=1000 <<EOF
  > let rec fact n = if n = 1 then 1 else n * fact (n - 1);; fact 4;;
  val fact = <fun>
  24

  $ ../bin/REPL.exe <<EOF
  > let x = 5
  val x = 5

  $ ../bin/REPL.exe <<EOF
  > let rec f x = f (x - 5)
  val f = <fun>

  $ ../bin/REPL.exe <<EOF
  > (fun x y -> x + y) 5 4
  9

  $ ../bin/REPL.exe -max-steps=10 <<EOF
  > Some (7 + 17);; None
  Some 24
  None

  $ ../bin/REPL.exe <<EOF
  > let rec x = 21 and y = x + 1;;
  val x = 21
  val y = 22

  $ ../bin/REPL.exe <<EOF
  > let x = let y = 1 and z = 2 and w = y + 3 in w;;
  val x = 4

  $ ../bin/REPL.exe <<EOF
  > let foo x y = x * y;; let q = foo 1 6;; let w = foo 2 (-5)
  val foo = <fun>
  val q = 6
  val w = -10

  $ ../bin/REPL.exe <<EOF
  > if 122 >= 221 then (print_int 122)
  ()

  $ ../bin/REPL.exe <<EOF
  > let () = (print_int 10);; let _ = 8 + 123
  10

  $ ../bin/REPL.exe <<EOF
  > let x = 1;; let x = 2;;
  val x = 2

  $ ../bin/REPL.exe <<EOF
  > (fun Some x -> x) Some 3
  3

  $ ../bin/REPL.exe <<EOF
  > -4 + ()
  Type error

  $ ../bin/REPL.exe <<EOF
  > if 122 then 1 else 2
  Type error

  $ ../bin/REPL.exe <<EOF
  > print_int true
  Type error

  $ ../bin/REPL.exe <<EOF
  > let Some x = None;;
  Type error

  $ ../bin/REPL.exe <<EOF
  > 1 2
  Type error

  $ ../bin/REPL.exe <<EOF
  > let rec () = 9
  Type error

  $ ../bin/REPL.exe <<EOF
  > if 6 > 5 then 7
  Type error

  $ ../bin/REPL.exe <<EOF
  > (fun Some x -> x) None
  Matching failure

  $ ../bin/REPL.exe <<EOF
  > if 4 then ()
  Type error

  $ ../bin/REPL.exe <<EOF
  > let x = y
  Undefined variable 'y'

  $ ../bin/REPL.exe <<EOF
  > 9 / 0
  Division by zero

  $ ../bin/REPL.exe <<EOF
  > let x = 1;; match x with | 0 -> x + 2;;
  Matching failure

  $ ../bin/REPL.exe -max-steps=5 <<EOF
  > let x = 1;; match x with | 0 -> x + 2 | 1 -> x + 3;;
  OutOfSteps

  $ ../bin/REPL.exe -max-steps=1000 <<EOF
  > let rec loop x = loop x in loop 0;;
  OutOfSteps

  $ ../bin/REPL.exe <<EOF
  > let loak ==0ih
  : end_of_input

  $ ../bin/REPL.exe <<EOF
  > let y = let x = 1 in x + 1;; x + 1;;
  Undefined variable 'x'

  $ ../bin/REPL.exe -max-steps=1000 <<EOF
  > let x = true in not x 
  false

  $ ../bin/REPL.exe -max-steps=1000 <<EOF
  > - (let x = true in not x)
  Type error

  $ ../bin/REPL.exe <<EOF
  > let x = 1;; x + 2;;
  val x = 1
  3

  $ ../bin/REPL.exe <<EOF
  > let x = 1;; match x with | 0 -> x + 2 | 1 -> x + 3;;
  val x = 1
  4

  $ ../bin/REPL.exe <<EOF
  > let x = let y = 5 in y + 1;;
  val x = 6

  $ ../bin/REPL.exe <<EOF
  > let f = function | 1 -> 1 | 2 -> 2;; f 1;;
  val f = <function>
  1

  $ ../bin/REPL.exe <<EOF
  > let (x : int) = 5;;
  val x = 5

  $ ../bin/REPL.exe <<EOF
  > let (x : int option) = Some 5;; match x with | Some val -> print_int (-val) | None -> ()
  -5
  val x = Some 5
  ()

  $ ../bin/REPL.exe <<EOF
  > if true then print_int 1
  1
  ()

  $ ../bin/REPL.exe <<EOF
  > print_int 42
  42
  ()

  $ ../bin/REPL.exe <<EOF
  > print_int
  <builtin>

  $ ../bin/REPL.exe <<EOF
  > let rec (fib : int -> int) n = if n<2 then n else fib (n - 1) + fib (n - 2);; fib 4
  val fib = <fun>
  3

  $ ../bin/REPL.exe <<EOF
  > let x = Some true;; let z = match x with | Some y -> (match y with | (true : bool) -> 1 | false -> 2) | _ -> 3;;
  val x = Some true
  val z = 1

  $ ../bin/REPL.exe <<EOF
  > let x1 = None;; let x2 = Some true;; let f x = match x with | None -> 3 | _ -> 8;; f x1 + f x2
  val x1 = None
  val x2 = Some true
  val f = <fun>
  11

  $ ../bin/REPL.exe <<EOF
  > let f = function | () -> None | _ -> None;; f (print_int 10)
  10
  val f = <function>
  None
