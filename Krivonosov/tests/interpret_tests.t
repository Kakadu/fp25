Copyright 2021-2024, Kakadu and contributors
SPDX-License-Identifier: CC0-1.0

Cram tests here. They run and compare program output to the expected output
https://dune.readthedocs.io/en/stable/tests.html#cram-tests
Use `dune promote` after you change things that should runned

If you need to put sample program and use it both in your interpreter and preinstalled one,
you could put it into separate file. Thise will need stanza `(cram (deps demo_input.txt))`
in the dune file

  $ ../bin/REPL.exe -cbv -dparsetree <<EOF
  > \f.x
  Parsed result: (Abs (f, (Var x)))
  Evaluated result: (λ _ . x)
  $ ../bin/REPL.exe -dparsetree <<EOF
  > garbage242
  Parsed result: (Var garbage242)
  Error: Unknown variable garbage242



  $ ../bin/REPL.exe -no -dparsetree <<EOF
  > (\x.\y.x)(\u.u)((\x. x x)(\x.x x))
  Parsed result: (App (
                    (App ((Abs (x, (Abs (y, (Var x))))), (Abs (u, (Var u))))),
                    (App ((Abs (x, (App ((Var x), (Var x))))),
                       (Abs (x, (App ((Var x), (Var x)))))))
                    ))
  Evaluated result: (λ u . u)
Below we redirect contents of the file to the evaluator
  $ ../bin/REPL.exe -dparsetree -stop-after parsing   < lam_1+1.txt
  Parsed result: (App (
                    (Abs (m,
                       (Abs (n,
                          (Abs (f,
                             (Abs (x,
                                (App ((Var m),
                                   (App ((Var f),
                                      (App ((Var n), (App ((Var f), (Var x)))))
                                      ))
                                   ))
                                ))
                             ))
                          ))
                       )),
                    (App ((Abs (f, (Abs (x, (App ((Var f), (Var x))))))),
                       (Abs (f, (Abs (x, (App ((Var f), (Var x)))))))))
                    ))

  $ ../bin/REPL.exe -ao   < lam_1+1.txt
  Evaluated result: (λ n f x _x -> ((f (n (f x))) _x))
  $ ../bin/REPL.exe -ao   < lam_2x1.txt
  Evaluated result: 2
Call by value doesn't reduce under abstraction
  $ ../bin/REPL.exe -cbv   < lam_2x1.txt
  Evaluated result: (λ z . (2 (1 z)))
  $ ../bin/REPL.exe -ao -small   < lam_3x2.txt
   -- ((λ y z -> ((λ f x -> (f (f (f x)))) (y z))) 2)
   -- ((λ y z x -> ((y z) ((y z) ((y z) x)))) 2)
   -- (λ z x -> ((2 z) ((2 z) ((2 z) x))))
   -- (λ z x -> ((λ x . (z (z x))) ((2 z) ((2 z) x))))
   -- (λ z x -> ((λ x . (z (z x))) ((λ x . (z (z x))) ((2 z) x))))
   -- (λ z x -> ((λ x . (z (z x))) ((λ x . (z (z x))) ((λ x . (z (z x))) x))))
   -- (λ z x -> ((λ x . (z (z x))) ((λ x . (z (z x))) (z (z x)))))
   -- (λ z x -> ((λ x . (z (z x))) (z (z (z (z x))))))
   -- (λ z x -> (z (z (z (z (z (z x)))))))
  Evaluated result: (λ z x -> (z (z (z (z (z (z x)))))))
  $ ../bin/REPL.exe -ao   < lam_zero.txt
  Evaluated result: ⊥
For 3! we use noral order reduction
  $ cat lam_fac3.txt
  (((λ f . ((λ x . (f (x x))) (λ x . (f (x x))))) (λ s . (λ n . ((((λ n . ((n (λ x . (λ x . (λ y . y)))) (λ x . (λ y . x)))) n) (λ f . (λ x . (f x)))) (((λ x . (λ y . (λ z . (x (y z))))) (s ((λ n . (λ f . (λ x . (((n (λ g . (λ h . (h (g f))))) (λ u . x)) (λ u . u))))) n))) n))))) (λ f . (λ x . (f (f (f x))))))
  $ ../bin/REPL.exe -no   < lam_fac3.txt
  Evaluated result: (λ z x -> (z (z (z (z (z (z x)))))))

If-then-else tests
Test parsing if-then-else
  $ ../bin/REPL.exe -dparsetree -stop-after parsing <<EOF
  > if 1 then 5 else 10
  Parsed result: (If ((Int 1), (Int 5), (Some (Int 10))))

Test parsing if-then without else
  $ ../bin/REPL.exe -dparsetree -stop-after parsing <<EOF
  > if 0 then 100
  Parsed result: (If ((Int 0), (Int 100), None))

Let-binding tests
Test parsing simple let
  $ ../bin/REPL.exe -dparsetree -stop-after parsing <<EOF
  > let x = 5 in x + 3
  Parsed result: (Let (false, x, (Int 5), (BinOp (Add, (Var x), (Int 3)))))

Test parsing nested let
  $ ../bin/REPL.exe -dparsetree -stop-after parsing <<EOF
  > let x = 10 in let y = 20 in x + y
  Parsed result: (Let (false, x, (Int 10),
                    (Let (false, y, (Int 20), (BinOp (Add, (Var x), (Var y)))))
                    ))

Test parsing let rec
  $ ../bin/REPL.exe -dparsetree -stop-after parsing <<EOF
  > let rec fact = \n. if n then n else 1 in fact 5
  Parsed result: (Let (true, fact,
                    (Abs (n, (If ((Var n), (Var n), (Some (Int 1)))))),
                    (App ((Var fact), (Int 5)))))

Interpreter tests
=================

Basic arithmetic
  $ ../bin/REPL.exe <<EOF
  > 2 + 3
  Evaluated result: 5
  $ ../bin/REPL.exe <<EOF
  > 10 - 4
  Evaluated result: 6
  $ ../bin/REPL.exe <<EOF
  > 3 * 4
  Evaluated result: 12
  $ ../bin/REPL.exe <<EOF
  > 15 / 3
  Evaluated result: 5
  $ ../bin/REPL.exe <<EOF
  > 17 % 5
  Evaluated result: 2

Comparison operators
  $ ../bin/REPL.exe <<EOF
  > 5 = 5
  Evaluated result: 1
  $ ../bin/REPL.exe <<EOF
  > 5 = 3
  Evaluated result: 0
  $ ../bin/REPL.exe <<EOF
  > 3 < 5
  Evaluated result: 1
  $ ../bin/REPL.exe <<EOF
  > 5 <= 5
  Evaluated result: 1

Conditional expressions
  $ ../bin/REPL.exe <<EOF
  > if 1 then 100 else 200
  Evaluated result: 100
  $ ../bin/REPL.exe <<EOF
  > if 0 then 100 else 200
  Evaluated result: 200
  $ ../bin/REPL.exe <<EOF
  > if (3 > 2) then 42 else 0
  Evaluated result: 42

Let bindings
  $ ../bin/REPL.exe <<EOF
  > let x = 10 in x + 5
  Evaluated result: 15
  $ ../bin/REPL.exe <<EOF
  > let x = 5 in let y = 10 in x * y
  Evaluated result: 50

Factorial with let rec
  $ ../bin/REPL.exe <<EOF
  > let rec fact = fun n -> if n <= 1 then 1 else n * fact (n - 1) in fact 0
  Evaluated result: 1
  $ ../bin/REPL.exe <<EOF
  > let rec fact = fun n -> if n <= 1 then 1 else n * fact (n - 1) in fact 1
  Evaluated result: 1
  $ ../bin/REPL.exe <<EOF
  > let rec fact = fun n -> if n <= 1 then 1 else n * fact (n - 1) in fact 5
  Evaluated result: 120
  $ ../bin/REPL.exe <<EOF
  > let rec fact = fun n -> if n <= 1 then 1 else n * fact (n - 1) in fact 10
  Evaluated result: 3628800

Fibonacci with let rec
  $ ../bin/REPL.exe <<EOF
  > let rec fib = fun n -> if n <= 1 then n else fib (n - 1) + fib (n - 2) in fib 0
  Evaluated result: 0
  $ ../bin/REPL.exe <<EOF
  > let rec fib = fun n -> if n <= 1 then n else fib (n - 1) + fib (n - 2) in fib 1
  Evaluated result: 1
  $ ../bin/REPL.exe <<EOF
  > let rec fib = fun n -> if n <= 1 then n else fib (n - 1) + fib (n - 2) in fib 7
  Evaluated result: 13
  $ ../bin/REPL.exe <<EOF
  > let rec fib = fun n -> if n <= 1 then n else fib (n - 1) + fib (n - 2) in fib 10
  Evaluated result: 55

Multi-parameter syntax sugar
  $ ../bin/REPL.exe <<EOF
  > (fun x y -> x + y) 2 3
  Evaluated result: 5
  $ ../bin/REPL.exe <<EOF
  > (fun x y z -> x + y + z) 1 2 3
  Evaluated result: 6

Print function
  $ ../bin/REPL.exe <<EOF
  > print 42
  42
  Evaluated result: ()
  $ ../bin/REPL.exe <<EOF
  > print (5 + 3)
  8
  Evaluated result: ()
  $ ../bin/REPL.exe <<EOF
  > let x = print 10 in print 20
  10
  20
  Evaluated result: ()

Error handling - Division by zero
  $ ../bin/REPL.exe <<EOF
  > 10 / 0
  Error: Division by zero

Error handling - Unknown variable
  $ ../bin/REPL.exe <<EOF
  > x + 5
  Error: Unknown variable x

Error handling - Type mismatch
  $ ../bin/REPL.exe <<EOF
  > (fun x -> x) + 5
  Error: Type mismatch

Step limit - Infinite loop with let rec
  $ ../bin/REPL.exe -max-steps 100 <<EOF
  > let rec loop = fun x -> loop x in loop 0
  Error: Step limit exceeded (max 100 steps)

Step limit - Factorial completes within limit
  $ ../bin/REPL.exe -max-steps 200 <<EOF
  > let rec fact = fun n -> if n <= 1 then 1 else n * fact (n - 1) in fact 5
  Evaluated result: 120
