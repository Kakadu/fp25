Copyright 2021-2026, Kakadu and Yackovlev Nickolay
SPDX-License-Identifier: CC0-1.0

  $ REPL=./../bin/REPL.exe

  $ $REPL <<EOF
  > 1 + 2 * 3
  (1 + (2 * 3))
  7

  $ $REPL <<EOF
  > 20 / 2
  (20 / 2)
  10

  $ $REPL <<EOF
  > -(1 + 2)
  (-(1 + 2))
  -3

  $ $REPL <<EOF
  > 1 = 1
  (1 = 1)
  1

  $ $REPL <<EOF
  > 1 <> 2
  (1 <> 2)
  1

  $ $REPL <<EOF
  > 5 > 10
  (5 > 10)
  0

  $ $REPL <<EOF
  > 5 >= 10
  (5 >= 10)
  0

  $ printf "1 \r\t + 2\n" | $REPL
  (1 + 2)
  3

  $ $REPL <<EOF
  > if 1 < 2 then 100 else 200
  (if (1 < 2) then 100 else 200)
  100

  $ $REPL <<EOF
  > if 0 then 1 else 0
  (if 0 then 1 else 0)
  0

  $ $REPL <<EOF
  > let x = 5 in let y = x + 1 in x * y
  (let x = 5 in (let y = (x + 1) in (x * y)))
  30

  $ $REPL <<EOF
  > let f = fun x y z -> x + y + z in f 1 2 3
  (let f = (fun x -> (fun y -> (fun z -> ((x + y) + z)))) in (((f 1) 2) 3))
  6

  $ $REPL <<EOF
  > print_int 0
  (print_int 0)
  0
  ()

  $ $REPL <<EOF
  > let _ = print_int 999 in 0
  (let _ = (print_int 999) in 0)
  999
  0

  $ $REPL <<EOF
  > print_int
  print_int
  <prim print_int>

  $ $REPL <<EOF
  > fix
  fix
  <prim fix>

  $ $REPL <<EOF
  > let rec fact n = if n <= 1 then 1 else n * fact (n - 1) in fact 5
  (let rec fact = (fun n -> (if (n <= 1) then 1 else (n * (fact (n - 1))))) in (fact 5))
  120

  $ $REPL <<EOF
  > let rec fib n = if n <= 1 then n else fib (n-1) + fib (n-2) in fib 10
  (let rec fib = (fun n -> (if (n <= 1) then n else ((fib (n - 1)) + (fib (n - 2))))) in (fib 10))
  55

  $ $REPL <<EOF
  > let fact = fix (fun self -> fun n -> if n = 0 then 1 else n * self (n - 1)) in fact 5
  (let fact = (fix (fun self -> (fun n -> (if (n = 0) then 1 else (n * (self (n - 1))))))) in (fact 5))
  120

  $ $REPL <<EOF
  > let rec loop x = loop x in loop 0
  (let rec loop = (fun x -> (loop x)) in (loop 0))
  Error: Out of fuel

  $ $REPL <<EOF
  > 10 / 0
  (10 / 0)
  Error: Division by zero

  $ $REPL <<EOF
  > unknown_var
  unknown_var
  Error: Unknown variable: unknown_var

  $ $REPL <<EOF
  > let x = 5 in x 10
  (let x = 5 in (x 10))
  Error: Not a function: 5

  $ $REPL <<EOF
  > (fun x -> x) + 1
  ((fun x -> x) + 1)
  Error: Type error: integer operands expected in arithmetic

  $ $REPL <<EOF
  > if (fun x -> x) then 1 else 0
  (if (fun x -> x) then 1 else 0)
  Error: Type error: if condition must be an int

  $ $REPL <<EOF
  > (fun x -> x) > 0
  ((fun x -> x) > 0)
  Error: Type error: comparison expects integer operands

  $ $REPL <<EOF
  > print_int (fun x -> x)
  (print_int (fun x -> x))
  Error: Type error: print_int expects int, got <fun>

  $ $REPL <<EOF
  > fix 5
  (fix 5)
  Error: Type error: fix expects a function, got 5

  $ $REPL <<EOF
  > fix (fun x -> 1)
  (fix (fun x -> 1))
  Error: Type error: fix expects a function that returns a function

  $ $REPL <<EOF
  > let rec x = 5 in x
  (let rec x = 5 in x)
  Error: Type error: let rec expects a function on the right-hand side

  $ $REPL <<EOF
  > 1 + * 2
  Error: : end_of_input

  $ $REPL <<EOF
  > -(fun x -> x)
  (-(fun x -> x))
  Error: Type error: integer operand expected in unary operation

  $ $REPL <<EOF
  > let if = 5 in 1
  Error: : no more choices

  $ $REPL <<EOF
  > letx = 0 in x
  Error: : end_of_input

  $ $REPL <<EOF
  > let x = 0 in(x)
  (let x = 0 in x)
  0

  $ $REPL <<EOF
  > let fix = fun f -> (fun x -> f (fun eta -> x x eta)) (fun x -> f (fun eta -> x x eta)) in
  > let fact = fun self n -> if n <= 1 then 1 else n * self (n - 1) in fix fact 5
  (let fix = (fun f -> ((fun x -> (f (fun eta -> ((x x) eta)))) (fun x -> (f (fun eta -> ((x x) eta)))))) in (let fact = (fun self -> (fun n -> (if (n <= 1) then 1 else (n * (self (n - 1)))))) in ((fix fact) 5)))
  120
