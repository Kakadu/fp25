Copyright 2021-2024, Kakadu and contributors
SPDX-License-Identifier: CC0-1.0

Cram tests here. They run and compare program output to the expected output
https://dune.readthedocs.io/en/stable/tests.html#cram-tests
Use `dune promote` after you change things that should runned

If you need to put sample program and use it both in your interpreter and preinstalled one,
you could put it into separate file. Thise will need stanza `(cram (deps demo_input.txt))`
in the dune file

  $ ../bin/REPL.exe <<EOF
  > 2 + 3
  Result: 5

  $ ../bin/REPL.exe <<EOF
  > 10 - 4
  Result: 6

  $ ../bin/REPL.exe <<EOF
  > 6 * 7
  Result: 42

  $ ../bin/REPL.exe <<EOF
  > 15 / 3
  Result: 5

  $ ../bin/REPL.exe <<EOF
  > 5 / 0
  Error: Division by zero

  $ ../bin/REPL.exe <<EOF
  > 5 = 5
  Result: 1

  $ ../bin/REPL.exe <<EOF
  > 5 <> 3
  Result: 1

  $ ../bin/REPL.exe <<EOF
  > 3 < 5
  Result: 1

  $ ../bin/REPL.exe <<EOF
  > if 1 then 10 else 20
  Result: 10

  $ ../bin/REPL.exe <<EOF
  > if 0 then 10 else 20
  Result: 20

  $ ../bin/REPL.exe <<EOF
  > fun x -> x * 2
  Result: <closure>

  $ ../bin/REPL.exe <<EOF
  > let x = 5 in x + 1
  Result: 6

  $ ../bin/REPL.exe <<EOF
  > let f = fun x -> x * 2 in f 10
  Result: 20

  $ ../bin/REPL.exe --ast <<EOF
  > 2 + 3
  AST: (2 + 3)
  Result: 5

  $ ../bin/REPL.exe <<EOF
  > fun x y -> x + y
  Result: <closure>

  $ ../bin/REPL.exe <<EOF
  > let add = fun x y -> x + y in add 3 4
  Result: 7

  $ ../bin/REPL.exe <<EOF
  > 2 + 3 * 4
  Result: 14

  $ ../bin/REPL.exe <<EOF
  > (2 + 3) * 4
  Result: 20

  $ ../bin/REPL.exe <<EOF
  > print 42
  42
  Result: ()

  $ ../bin/REPL.exe <<EOF
  > print (5 + 3)
  8
  Result: ()

  $ ../bin/REPL.exe --ast --maxSteps=10000 <<EOF
  > let rec fact = fun n -> if n = 0 then 1 else n * fact (n - 1) in fact 5
  AST: (let rec fact = (fun n -> (if (n = 0) then 1 else (n * (fact (n - 1))))) in (fact 5))
  Result: 120

  $ ../bin/REPL.exe --maxSteps=10000 <<EOF
  > let rec fact n = if n = 1 then 1 else n * fact (n - 1) in fact 5
  Result: 120

  $ ../bin/REPL.exe --ast --maxSteps=10000 <<EOF
  > let rec fact n = if n = 1 then 1 else n * fact (n - 1) in fact 5
  AST: (let rec fact = (fun n -> (if (n = 1) then 1 else (n * (fact (n - 1))))) in (fact 5))
  Result: 120

  $ ../bin/REPL.exe --ast <<EOF
  > let rec fix = fun f ->  (fun x -> f (fun y -> (x x) y))  (fun x -> f (fun y -> (x x) y)) in let fact = fix (fun fact -> fun n -> if n = 0 then 1 else n * fact (n - 1)) in fact 5
  AST: (let rec fix = (fun f -> ((fun x -> (f (fun y -> ((x x) y)))) (fun x -> (f (fun y -> ((x x) y)))))) in (let fact = (fix (fun fact -> (fun n -> (if (n = 0) then 1 else (n * (fact (n - 1))))))) in (fact 5)))
  Result: 120

  $ ../bin/REPL.exe --maxSteps=10000 <<EOF
  > let rec fib n = if n <= 1 then n else fib (n - 1) + fib (n - 2) in fib 6
  Result: 8

  $ ../bin/REPL.exe --maxSteps=50 <<EOF
  > let rec loop x = loop x in loop 1
  Error: Maximum steps exceeded

  $ ../bin/REPL.exe <<EOF
  > let x = 10 in let f = fun y -> x + y in let x = 20 in f 5
  Result: 15

  $ ../bin/REPL.exe <<EOF
  > let rec sum n = if n = 0 then 0 else n + sum (n - 1) in sum 5
  Result: 15

  $ ../bin/REPL.exe <<EOF
  > x + 1
  Error: Unbound variable: x

  $ ../bin/REPL.exe <<EOF
  > 5 10
  Error: Not a function

  $ ../bin/REPL.exe <<EOF
  > if fun x -> x then 1 else 2
  Error: Type error: If condition must be integer

  $ ../bin/REPL.exe --ast <<EOF
  > 1 + 2
  AST: (1 + 2)
  Result: 3

  $ ../bin/REPL.exe --maxSteps=5 <<EOF
  > let rec loop x = loop (x + 1) in loop 0
  Error: Maximum steps exceeded

  $ ../bin/REPL.exe <<EOF
  > let x = 1 in let y = 2 in let z = 3 in x + y + z
  Result: 6

  $ ../bin/REPL.exe <<EOF
  > let apply_twice f x = f (f x) in let inc x = x + 1 in apply_twice inc 5
  Result: 7

  $ ../bin/REPL.exe <<EOF
  > true
  Result: 1

  $ ../bin/REPL.exe <<EOF
  > false
  Result: 0

  $ ../bin/REPL.exe <<EOF
  > if true then 100 else 200
  Result: 100

  $ ../bin/REPL.exe <<EOF
  > if false then 100 else 200
  Result: 200

  $ ../bin/REPL.exe <<EOF
  > -5
  Result: -5

  $ ../bin/REPL.exe <<EOF
  > -x
  Error: Unbound variable: x

  $ ../bin/REPL.exe <<EOF
  > let x = 5 in -x
  Result: -5

  $ ../bin/REPL.exe <<EOF
  > 5 - -3
  Result: 8

  $ ../bin/REPL.exe <<EOF
  > -5 + 3
  Result: -2

  $ ../bin/REPL.exe <<EOF
  > 5 * -3
  Result: -15

  $ ../bin/REPL.exe <<EOF
  > -10 / 2
  Result: -5

  $ ../bin/REPL.exe <<EOF
  > -(3 + 4)
  Result: -7

  $ ../bin/REPL.exe <<EOF
  > -(-5)
  Result: 5

  $ ../bin/REPL.exe <<EOF
  > 2 * -3 + 4
  Result: -2

  $ ../bin/REPL.exe <<EOF
  > let x = 3 in let y = -x in y + 2
  Result: -1

  $ ../bin/REPL.exe <<EOF
  > -5 < 0
  Result: 1

  $ ../bin/REPL.exe <<EOF
  > 0 > -5
  Result: 1

  $ ../bin/REPL.exe <<EOF
  > -3 = -3
  Result: 1

  $ ../bin/REPL.exe <<EOF
  > -3 = 3
  Result: 0

  $ ../bin/REPL.exe --ast <<EOF
  > -5
  AST: (-5)
  Result: -5

  $ ../bin/REPL.exe --ast <<EOF
  > 5 - -3
  AST: (5 - (-3))
  Result: 8

  $ ../bin/REPL.exe <<EOF
  > f -x
  Error: Unbound variable: f

  $ ../bin/REPL.exe <<EOF
  > (fun x -> -x) 5
  Result: -5

  $ ../bin/REPL.exe <<EOF
  > let neg = fun x -> -x in neg 7
  Result: -7

  $ ../bin/REPL.exe <<EOF
  > let rec sum_neg_ones n = if n = 0 then 0 else -1 + sum_neg_ones (n - 1) in sum_neg_ones 5
  Result: -5
