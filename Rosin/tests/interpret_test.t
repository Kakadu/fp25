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
  AST here:
  (Inc(Int(42)))
  Interpretation result here:
  Ok: 43

  $ ../bin/REPL.exe <<EOF
  > --42
  AST here:
  (Dec(Int(42)))
  Interpretation result here:
  Ok: 41

  $ ../bin/REPL.exe <<EOF
  > 42 + 2
  AST here:
  (Plus(Int(42), Int(2)))
  Interpretation result here:
  Ok: 44

  $ ../bin/REPL.exe <<EOF
  > 42 - 2
  AST here:
  (Minus(Int(42), Int(2)))
  Interpretation result here:
  Ok: 40

  $ ../bin/REPL.exe <<EOF
  > 42 / 2
  AST here:
  (Div(Int(42), Int(2)))
  Interpretation result here:
  Ok: 21

  $ ../bin/REPL.exe <<EOF
  > 42 * 2
  AST here:
  (Mult(Int(42), Int(2)))
  Interpretation result here:
  Ok: 84

  $ ../bin/REPL.exe <<EOF
  > 42 + 2 * 2
  AST here:
  (Plus(Int(42), (Mult(Int(2), Int(2)))))
  Interpretation result here:
  Ok: 46

  $ ../bin/REPL.exe <<EOF
  > (42 + 2) * 2
  AST here:
  (Mult((Plus(Int(42), Int(2))), Int(2)))
  Interpretation result here:
  Ok: 88

  $ ../bin/REPL.exe <<EOF
  > 42 / 0
  AST here:
  (Div(Int(42), Int(0)))
  Interpretation result here:
  Error!: DivisionByZero

  $ ../bin/REPL.exe <<EOF
  > if 5 then 10 else 20
  AST here:
  If(Int(5)) Then(Int(10)) Else (Int(20)))
  Interpretation result here:
  Ok: 10

  $ ../bin/REPL.exe <<EOF
  > if -5 then 10 else 20
  AST here:
  If(Int(-5)) Then(Int(10)) Else (Int(20)))
  Interpretation result here:
  Ok: 20

  $ ../bin/REPL.exe <<EOF
  > if 5 then 10
  AST here:
  If(Int(5)) Then(Int(10))
  Interpretation result here:
  Ok: 10

  $ ../bin/REPL.exe <<EOF
  > let x = 5 in x + 1
  AST here:
  Let(x, Int(5)) in (Plus(Var(x), Int(1)))
  Interpretation result here:
  Ok: 6

  $ ../bin/REPL.exe <<EOF
  > let x = 5 in let y = x + 1 in y * 2
  AST here:
  Let(x, Int(5)) in Let(y, (Plus(Var(x), Int(1)))) in (Mult(Var(y), Int(2)))
  Interpretation result here:
  Ok: 12

  $ ../bin/REPL.exe <<EOF
  > let f = fun x -> x * 2 in f 10
  AST here:
  Let(f, Fun(x, (Mult(Var(x), Int(2))))) in (App(Var(f), Int(10)))
  Interpretation result here:
  Ok: 20

  $ ../bin/REPL.exe <<EOF
  > let f = fun x y z -> x + y + z in f 1 2 3
  AST here:
  Let(f, Fun(x, Fun(y, Fun(z, (Plus((Plus(Var(x), Var(y))), Var(z))))))) in (App((App((App(Var(f), Int(1))), Int(2))), Int(3)))
  Interpretation result here:
  Ok: 6

  $ ../bin/REPL.exe <<EOF
  > let rec fact = fun n -> if n then n * fact (n - 1) else 1 in fact 5
  AST here:
  Letrec((fact, Fun(n, If(Var(n)) Then((Mult(Var(n), (App(Var(fact), (Minus(Var(n), Int(1)))))))) Else (Int(1))))) in (App(Var(fact), Int(5))))
  Interpretation result here:
  Ok: 120

  $ ../bin/REPL.exe <<EOF
  > let rec fib = fun n -> if n - 1 then (if n - 2 then fib (n - 1) + fib (n - 2) else 1) else n in fib 6
  AST here:
  Letrec((fib, Fun(n, If((Minus(Var(n), Int(1)))) Then(If((Minus(Var(n), Int(2)))) Then((Plus((App(Var(fib), (Minus(Var(n), Int(1))))), (App(Var(fib), (Minus(Var(n), Int(2)))))))) Else (Int(1)))) Else (Var(n))))) in (App(Var(fib), Int(6))))
  Interpretation result here:
  Ok: 8

  $ ../bin/REPL.exe <<EOF
  > let rec sum = fun n -> if n then n + sum (n - 1) else 0 in sum 5
  AST here:
  Letrec((sum, Fun(n, If(Var(n)) Then((Plus(Var(n), (App(Var(sum), (Minus(Var(n), Int(1)))))))) Else (Int(0))))) in (App(Var(sum), Int(5))))
  Interpretation result here:
  Ok: 15

  $ ../bin/REPL.exe <<EOF
  > print 42
  42
  AST here:
  Print(Int(42))
  Interpretation result here:
  Ok: 42

  $ ../bin/REPL.exe <<EOF
  > print (1 + 2)
  3
  AST here:
  Print((Plus(Int(1), Int(2))))
  Interpretation result here:
  Ok: 3

  $ ../bin/REPL.exe --steps=500 <<EOF
  > let rec infinite = fun x -> infinite x in infinite 1
  AST here:
  Letrec((infinite, Fun(x, (App(Var(infinite), Var(x))))) in (App(Var(infinite), Int(1))))
  Interpretation result here:
  Error!: StepLimitExceeded

  $ ../bin/REPL.exe <<EOF
  > let fact = fix (fun f n -> if n then n * (f (n - 1)) else 1) in fact 5
  AST here:
  Let(fact, Fix(Fun(f, Fun(n, If(Var(n)) Then((Mult(Var(n), (App(Var(f), (Minus(Var(n), Int(1)))))))) Else (Int(1))))))) in (App(Var(fact), Int(5)))
  Interpretation result here:
  Ok: 120
