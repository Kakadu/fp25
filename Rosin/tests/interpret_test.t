Copyright 2021-2025, Kakadu and contributors
SPDX-License-Identifier: CC0-1.0

Cram tests here. They run and compare program output to the expected output
https://dune.readthedocs.io/en/stable/tests.html#cram-tests
Use `dune promote` after you change things that should runned

If you need to put sample program and use it both in your interpreter and preinstalled one,
you could put it into separate file. Thise will need stanza `(cram (deps demo_input.txt))`
in the dune file

  $ ../bin/REPL.exe <<EOF
  > ++42
  Fatal error: exception Failure(": no more choices")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Dune__exe__REPL in file "bin/REPL.ml", line 56, characters 36-48
  [2]

  $ ../bin/REPL.exe <<EOF
  > --42
  Fatal error: exception Failure(": no more choices")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Dune__exe__REPL in file "bin/REPL.ml", line 56, characters 36-48
  [2]

  $ ../bin/REPL.exe <<EOF
  > 42 + 2
  AST here:
  (42 + 2)
  Interpretation result here:
  Ok: 44

  $ ../bin/REPL.exe <<EOF
  > 42 - 2
  AST here:
  (42 - 2)
  Interpretation result here:
  Ok: 40

  $ ../bin/REPL.exe <<EOF
  > 42 / 2
  AST here:
  (42 / 2)
  Interpretation result here:
  Ok: 21

  $ ../bin/REPL.exe <<EOF
  > 42 * 2
  AST here:
  (42 * 2)
  Interpretation result here:
  Ok: 84

  $ ../bin/REPL.exe <<EOF
  > 42 + 2 * 2
  AST here:
  (42 + (2 * 2))
  Interpretation result here:
  Ok: 46

  $ ../bin/REPL.exe <<EOF
  > (42 + 2) * 2
  AST here:
  ((42 + 2) * 2)
  Interpretation result here:
  Ok: 88

  $ ../bin/REPL.exe <<EOF
  > 42 / 0
  AST here:
  (42 / 0)
  Interpretation result here:
  Error!: DivisionByZero

  $ ../bin/REPL.exe <<EOF
  > if 5 then 10 else 20
  AST here:
  if 5 then 10 else 20
  Interpretation result here:
  Ok: 10

  $ ../bin/REPL.exe <<EOF
  > if -5 then 10 else 20
  AST here:
  if -5 then 10 else 20
  Interpretation result here:
  Ok: 20

  $ ../bin/REPL.exe <<EOF
  > if 5 then 10
  AST here:
  if 5 then 10
  Interpretation result here:
  Ok: 10

  $ ../bin/REPL.exe <<EOF
  > let x = 5 in x + 1
  AST here:
  let x = 5 in (x + 1)
  Interpretation result here:
  Ok: 6

  $ ../bin/REPL.exe <<EOF
  > let x = 5 in let y = x + 1 in y * 2
  AST here:
  let x = 5 in let y = (x + 1) in (y * 2)
  Interpretation result here:
  Ok: 12

  $ ../bin/REPL.exe <<EOF
  > let f = fun x -> x * 2 in f 10
  AST here:
  let f = fun x -> (x * 2) in f (10)
  Interpretation result here:
  Ok: 20

  $ ../bin/REPL.exe <<EOF
  > let f = fun x y z -> x + y + z in f 1 2 3
  AST here:
  let f = fun x -> fun y -> fun z -> ((x + y) + z) in f (1) (2) (3)
  Interpretation result here:
  Ok: 6

  $ ../bin/REPL.exe <<EOF
  > let rec fact = fun n -> if n then n * fact (n - 1) else 1 in fact 5
  AST here:
  let rec fact = fun n -> if n then (n * fact ((n - 1))) else 1 in fact (5)
  Interpretation result here:
  Ok: 120

  $ ../bin/REPL.exe <<EOF
  > let rec fib = fun n -> if n - 1 then (if n - 2 then fib (n - 1) + fib (n - 2) else 1) else n in fib 6
  AST here:
  let rec fib = fun n -> if (n - 1) then if (n - 2) then (fib ((n - 1)) + fib ((n - 2))) else 1 else n in fib (6)
  Interpretation result here:
  Ok: 8

  $ ../bin/REPL.exe <<EOF
  > let rec sum = fun n -> if n then n + sum (n - 1) else 0 in sum 5
  AST here:
  let rec sum = fun n -> if n then (n + sum ((n - 1))) else 0 in sum (5)
  Interpretation result here:
  Ok: 15

  $ ../bin/REPL.exe <<EOF
  > print 42
  42
  AST here:
  print (42)
  Interpretation result here:
  Ok: 42

  $ ../bin/REPL.exe <<EOF
  > print (1 + 2)
  3
  AST here:
  print ((1 + 2))
  Interpretation result here:
  Ok: 3

  $ ../bin/REPL.exe --steps=500 <<EOF
  > let rec infinite = fun x -> infinite x in infinite 1
  AST here:
  let rec infinite = fun x -> infinite (x) in infinite (1)
  Interpretation result here:
  Error!: StepLimitExceeded

  $ ../bin/REPL.exe <<EOF
  > let rec fix f eta = f (fix f) eta in let fact_gen = fun fact n -> if n = 0 then 1 else n * fact (n-1) in let fact = fix fact_gen in fact 5
  AST here:
  let rec fix = fun f -> fun eta -> f (fix (f)) (eta) in let fact_gen = fun fact -> fun n -> if (n = 0) then 1 else (n * fact ((n - 1))) in let fact = fix (fact_gen) in fact (5)
  Interpretation result here:
  Ok: 120
