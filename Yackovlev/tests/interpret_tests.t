Copyright 2021-2025, Kakadu and contributors
SPDX-License-Identifier: CC0-1.0

Cram tests here. They run and compare program output to the expected output
https://dune.readthedocs.io/en/stable/tests.html#cram-tests
Use `dune promote` after you change things that should runned

If you need to put sample program and use it both in your interpreter and preinstalled one,
you could put it into separate file. Thise will need stanza `(cram (deps demo_input.txt))`
in the dune file

  $ REPL=./../bin/REPL.exe

  $ $REPL <<EOF
  > 1 + 2 * 3
  (1 + (2 * 3))
  7

  $ $REPL <<EOF
  > 10 / 0
  (10 / 0)
  Error: Division by zero

  $ $REPL <<EOF
  > if 1 < 2 then 100 else 200
  (if (1 < 2) then 100 else 200)
  100

  $ $REPL <<EOF
  > if 5 then 1 else 0
  (if 5 then 1 else 0)
  1

  $ $REPL <<EOF
  > if 0 then 100 else 200
  (if 0 then 100 else 200)
  200

  $ $REPL <<EOF
  > let x = 5 in let y = x + 1 in x * y
  (let x = 5 in (let y = (x + 1) in (x * y)))
  30

  $ $REPL <<EOF
  > let f = fun x y z -> x + y + z in f 1 2 3
  (let f = (fun x -> (fun y -> (fun z -> ((x + y) + z)))) in (((f 1) 2) 3))
  6

  $ $REPL <<EOF
  > let rec fact n = if n <= 1 then 1 else n * fact (n - 1) in fact 5
  (let rec fact = (fun n -> (if (n <= 1) then 1 else (n * (fact (n - 1))))) in (fact 5))
  120

  $ $REPL <<EOF
  > let rec fib n = if n <= 1 then n else fib (n-1) + fib (n-2) in fib 10
  (let rec fib = (fun n -> (if (n <= 1) then n else ((fib (n - 1)) + (fib (n - 2))))) in (fib 10))
  55

  $ $REPL <<EOF
  > let rec fix f x = f (fix f) x in
  > let fact_gen fact n = if n = 0 then 1 else n * fact (n - 1) in
  > let fact = fix fact_gen in
  > fact 5
  (let rec fix = (fun f -> (fun x -> ((f (fix f)) x))) in (let fact_gen = (fun fact -> (fun n -> (if (n = 0) then 1 else (n * (fact (n - 1)))))) in (let fact = (fix fact_gen) in (fact 5))))
  120

  $ $REPL <<EOF
  > let rec loop x = loop x in loop 0
  (let rec loop = (fun x -> (loop x)) in (loop 0))
  Error: Out of fuel

  $ $REPL <<EOF
  > let _ = print_int 999 in 0
  (let _ = (print_int 999) in 0)
  999
  0

  $ $REPL <<EOF
  > -5 + 3
  ((-5) + 3)
  -2

  $ $REPL <<EOF
  > let x = 10 in let x = 20 in x
  (let x = 10 in (let x = 20 in x))
  20

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
  > 5 >= 5
  (5 >= 5)
  1

  $ $REPL <<EOF
  > 2 <= 2
  (2 <= 2)
  1

  $ $REPL <<EOF
  > 3 <= 2
  (3 <= 2)
  0

  $ $REPL <<EOF
  > let fact = fix (fun self -> fun n -> if n = 0 then 1 else n * self (n - 1)) in fact 5
  (let fact = (fix (fun self -> (fun n -> (if (n = 0) then 1 else (n * (self (n - 1))))))) in (fact 5))
  120

  $ $REPL <<EOF
  > fix 5
  (fix 5)
  Error: Type error: fix expects a function, got 5

  $ $REPL <<EOF
  > fix (fun x -> 1)
  (fix (fun x -> 1))
  Error: Type error: fix expects a function that returns a function

  $ $REPL <<EOF
  > print_int (fun x -> x)
  (print_int (fun x -> x))
  Error: Type error: print_int expects int, got <fun>

  $ $REPL <<EOF
  > (fun x -> x) > 0
  ((fun x -> x) > 0)
  Error: Type error: comparison expects integer operands

  $ $REPL <<EOF
  > -(1 + 2)
  (0 - (1 + 2))
  -3

  $ $REPL <<EOF
  > let x = 5 in --x
  (let x = 5 in (0 - (0 - x)))
  5
