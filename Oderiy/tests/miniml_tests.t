(* Copyright 2025, XRenso *)
(* SPDX-License-Identifier: CC0-1.0 *)

Cram tests for miniML interpreter

Test factorial calculation (5! = 120)
  $ cat factorial.miniml
  let rec fac n =
    if n <= 1 then 1 else n * fac (n - 1)
  in fac 5
  $ cat factorial.miniml | ../bin/REPL.exe
  Success: 120

Test fibonacci calculation (fib 10 = 55)
  $ cat fibonacci.miniml
  let rec fib n =
    if n <= 1 then n else fib (n - 1) + fib (n - 2)
  in fib 10

  $ cat fibonacci.miniml | ../bin/REPL.exe
  Success: 55

Test simple arithmetic
  $ echo "2 + 3 * 4" | ../bin/REPL.exe
  Success: 14

  $ echo "(2 + 3) * 4" | ../bin/REPL.exe
  Success: 20

Test let bindings
  $ echo "let x = 10 in let y = 20 in x + y" | ../bin/REPL.exe
  Success: 30

Test if expressions
  $ echo "if 5 > 3 then 100 else 200" | ../bin/REPL.exe
  Success: 100

  $ echo "if 2 >= 10 then 1 else 0" | ../bin/REPL.exe
  Success: 0

Test functions
  $ echo "let double x = x * 2 in double 21" | ../bin/REPL.exe
  Success: 42

  $ echo "(fun x -> x + 1) 41" | ../bin/REPL.exe
  Success: 42

Test fix combinator (built-in)
  $ cat builtin_fix.miniml
  let fact = fix (fun f -> fun n -> if n > 0 then f (n - 1) * n else 1)
  in fact 10
  $ cat builtin_fix.miniml | ../bin/REPL.exe
  Success: 3628800

Test fix combinator (manual implementation)
  $ cat fix_factorial.miniml
  let fix = fun f -> (fun x -> f (fun v -> x x v)) (fun x -> f (fun v -> x x v))
  in let fact = fix (fun f -> fun n -> if n > 0 then f (n - 1) * n else 1)
  in fact 5
  $ cat fix_factorial.miniml | ../bin/REPL.exe
  Success: 120

Test built-in print function
  $ echo "print 42" | ../bin/REPL.exe
  42
  Success: 42

  $ echo "let x = print 10 in x + 1" | ../bin/REPL.exe
  10
  Success: 11

Test user can redefine fix
  $ echo "let fix = 42 in fix" | ../bin/REPL.exe
  Success: 42

Test user can redefine print
  $ echo "let print = fun x -> x + 1 in print 5" | ../bin/REPL.exe
  Success: 6

Test letrec without space is identifier
  $ echo "let letrec = 7 in letrec" | ../bin/REPL.exe
  Success: 7

Test division by zero
  $ echo "1 / 0" | ../bin/REPL.exe
  Error: Division by zero
  [1]

Test unbound variable
  $ echo "unknown" | ../bin/REPL.exe
  Error: Unbound variable: unknown
  [1]

Test applying non-function
  $ echo "42 1" | ../bin/REPL.exe
  Error: applying non-function
  [1]
