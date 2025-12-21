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
