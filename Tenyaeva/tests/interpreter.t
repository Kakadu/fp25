(** Copyright 2025, Tenyaeva Ekaterina *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

  $ ../bin/REPL.exe <<EOF
  > 122
  - = 122

  $ ../bin/REPL.exe <<EOF
  > (08 + 122 / 2) * 3 - 4
  - = 203

  $ ../bin/REPL.exe <<EOF
  > -4 + 8 + (+3)
  - = 7

  $ ../bin/REPL.exe <<EOF
  > (4 <= 8) = true
  - = true

  $ ../bin/REPL.exe <<EOF
  > let x = false;; not x;;
  val x = false
  - = true

  $ ../bin/REPL.exe <<EOF
  > if 6 < 10 then 1 else 2;;
  - = 1

  $ ../bin/REPL.exe -max-steps=1000 <<EOF
  > let rec fact n = if n = 1 then 1 else n * fact (n - 1);; fact 4;;
  val fact = <fun>
  - = 24

  $ ../bin/REPL.exe <<EOF
  > let x = 5
  val x = 5

  $ ../bin/REPL.exe <<EOF
  > let rec f x = f (x - 5)
  val f = <fun>

  $ ../bin/REPL.exe <<EOF
  > (fun x y -> x + y) 5 4
  - = 9

  $ ../bin/REPL.exe -max-steps=10 <<EOF
  > Some(7 + 17)
  - = Some 24

  $ ../bin/REPL.exe <<EOF
  > let rec x = 21 and y = x + 1;;
  val x = 21
  val y = 22

  $ ../bin/REPL.exe <<EOF
  > let foo x y = x * y;; let q = foo 1 6;; let w = foo 2 (-5)
  val foo = <fun>
  val q = 6
  val w = -10

  $ ../bin/REPL.exe <<EOF
  > -4 + ()
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
  > let rec loop x = loop x;; loop 0;;
  OutOfSteps

  $ ../bin/REPL.exe <<EOF
  > let x = 1;; x + 2;;
  val x = 1
  - = 3

  $ ../bin/REPL.exe <<EOF
  > let x = 1;; match x with | 0 -> x + 2 | 1 -> x + 3;;
  val x = 1
  - = 4

  $ ../bin/REPL.exe <<EOF
  > let x = let y = 5 in y + 1;;
  val x = 6

  $ ../bin/REPL.exe <<EOF
  > let f = function | 1 -> 1 | 2 -> 2;; f 1;;
  val f = <function>
  - = 1

  $ ../bin/REPL.exe <<EOF
  > let (x : int) = 5;;
  val x = 5
