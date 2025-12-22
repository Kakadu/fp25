Copyright 2021-2024, Kakadu and contributors
SPDX-License-Identifier: CC0-1.0

Cram tests here. They run and compare program output to the expected output
https://dune.readthedocs.io/en/stable/tests.html#cram-tests
Use `dune promote` after you change things that should runned

miniML Interpreter tests
========================

Basic arithmetic
  $ ../bin/REPL.exe <<EOF
  > 2 + 3
  5
  $ ../bin/REPL.exe <<EOF
  > 10 - 4
  6
  $ ../bin/REPL.exe <<EOF
  > 3 * 4
  12
  $ ../bin/REPL.exe <<EOF
  > 15 / 3
  5
  $ ../bin/REPL.exe <<EOF
  > 17 % 5
  2

Comparison operators
  $ ../bin/REPL.exe <<EOF
  > 5 = 5
  1
  $ ../bin/REPL.exe <<EOF
  > 5 = 3
  0
  $ ../bin/REPL.exe <<EOF
  > 5 <> 3
  1
  $ ../bin/REPL.exe <<EOF
  > 5 <> 5
  0
  $ ../bin/REPL.exe <<EOF
  > 3 < 5
  1
  $ ../bin/REPL.exe <<EOF
  > 5 < 3
  0
  $ ../bin/REPL.exe <<EOF
  > 5 > 3
  1
  $ ../bin/REPL.exe <<EOF
  > 3 > 5
  0
  $ ../bin/REPL.exe <<EOF
  > 5 <= 5
  1
  $ ../bin/REPL.exe <<EOF
  > 6 <= 5
  0
  $ ../bin/REPL.exe <<EOF
  > 5 >= 5
  1
  $ ../bin/REPL.exe <<EOF
  > 4 >= 5
  0

Conditional expressions
  $ ../bin/REPL.exe <<EOF
  > if 1 then 100 else 200
  100
  $ ../bin/REPL.exe <<EOF
  > if 0 then 100 else 200
  200
  $ ../bin/REPL.exe <<EOF
  > if (3 > 2) then 42 else 0
  42
  $ ../bin/REPL.exe <<EOF
  > if 1 then 42
  42
  $ ../bin/REPL.exe <<EOF
  > if 0 then 42
  0

Let bindings
  $ ../bin/REPL.exe <<EOF
  > let x = 10 in x + 5
  15
  $ ../bin/REPL.exe <<EOF
  > let x = 5 in let y = 10 in x * y
  50
  $ ../bin/REPL.exe <<EOF
  > let rec x = 42 in x
  42

Factorial with let rec
  $ ../bin/REPL.exe <<EOF
  > let rec fact = fun n -> if n <= 1 then 1 else n * fact (n - 1) in fact 0
  1
  $ ../bin/REPL.exe <<EOF
  > let rec fact = fun n -> if n <= 1 then 1 else n * fact (n - 1) in fact 1
  1
  $ ../bin/REPL.exe <<EOF
  > let rec fact = fun n -> if n <= 1 then 1 else n * fact (n - 1) in fact 5
  120
  $ ../bin/REPL.exe <<EOF
  > let rec fact = fun n -> if n <= 1 then 1 else n * fact (n - 1) in fact 10
  3628800

Fibonacci with let rec
  $ ../bin/REPL.exe <<EOF
  > let rec fib = fun n -> if n <= 1 then n else fib (n - 1) + fib (n - 2) in fib 0
  0
  $ ../bin/REPL.exe <<EOF
  > let rec fib = fun n -> if n <= 1 then n else fib (n - 1) + fib (n - 2) in fib 1
  1
  $ ../bin/REPL.exe <<EOF
  > let rec fib = fun n -> if n <= 1 then n else fib (n - 1) + fib (n - 2) in fib 7
  13
  $ ../bin/REPL.exe <<EOF
  > let rec fib = fun n -> if n <= 1 then n else fib (n - 1) + fib (n - 2) in fib 10
  55

Multi-parameter syntax sugar
  $ ../bin/REPL.exe <<EOF
  > (fun x y -> x + y) 2 3
  5
  $ ../bin/REPL.exe <<EOF
  > (fun x y z -> x + y + z) 1 2 3
  6

Print function
  $ ../bin/REPL.exe <<EOF
  > print 42
  42
  ()
  $ ../bin/REPL.exe <<EOF
  > print (5 + 3)
  8
  ()
  $ ../bin/REPL.exe <<EOF
  > let x = print 10 in print 20
  10
  20
  ()
  $ ../bin/REPL.exe <<EOF
  > print (fun x -> x)
  <fun>
  ()
  $ ../bin/REPL.exe <<EOF
  > print print
  <builtin:print>
  ()

Returning closures and builtins
  $ ../bin/REPL.exe <<EOF
  > fun x -> x
  <fun>
  $ ../bin/REPL.exe <<EOF
  > print
  <builtin:print>

Error handling - Division by zero
  $ ../bin/REPL.exe <<EOF
  > 10 / 0
  Division by zero
  [1]

Error handling - Modulo by zero
  $ ../bin/REPL.exe <<EOF
  > 10 % 0
  Division by zero
  [1]

Error handling - Unknown variable
  $ ../bin/REPL.exe <<EOF
  > x + 5
  Unbound variable: x
  [1]

Error handling - Type mismatch
  $ ../bin/REPL.exe <<EOF
  > (fun x -> x) + 5
  Type error
  [1]
  $ ../bin/REPL.exe <<EOF
  > 5 + (fun x -> x)
  Type error
  [1]
  $ ../bin/REPL.exe <<EOF
  > 42 10
  Type error
  [1]
  $ ../bin/REPL.exe <<EOF
  > if (fun x -> x) then 1 else 0
  Type error
  [1]

Step limit - Infinite loop with let rec
  $ ../bin/REPL.exe -max-steps 100 <<EOF
  > let rec loop = fun x -> loop x in loop 0
  Step limit exceeded
  [1]

Step limit - Factorial completes within limit
  $ ../bin/REPL.exe -max-steps 200 <<EOF
  > let rec fact = fun n -> if n <= 1 then 1 else n * fact (n - 1) in fact 5
  120

REPL argument parsing edge cases
=================================

Invalid -max-steps value (non-numeric)
  $ ../bin/REPL.exe -max-steps invalid <<EOF
  > 42
  Error: Invalid value for -max-steps: invalid
  [1]

Unknown argument
  $ ../bin/REPL.exe -unknown-arg <<EOF
  > 42
  Error: Unknown argument: -unknown-arg
  [1]

Lambda syntax - Unicode λ
  $ ../bin/REPL.exe <<EOF
  > λx . x + 1
  <fun>

Lambda syntax - Backslash
  $ ../bin/REPL.exe <<EOF
  > \x . x + 1
  <fun>

Multi-parameter lambda with unicode
  $ ../bin/REPL.exe <<EOF
  > λx y -> x + y
  <fun>

Multi-parameter lambda with backslash
  $ ../bin/REPL.exe <<EOF
  > \x y -> x + y
  <fun>

Parsing error handling
  $ ../bin/REPL.exe <<EOF
  > this is invalid syntax
  Unbound variable: this
  [1]

Syntax error - unmatched parenthesis
  $ ../bin/REPL.exe <<EOF
  > (5 + 3
  Error: : no more choices
  [1]

Syntax error - missing closing paren with complex expr
  $ ../bin/REPL.exe <<EOF
  > (fun x -> x + 1
  Error: : no more choices
  [1]
