Copyright 2021-2024, Kakadu and contributors
SPDX-License-Identifier: CC0-1.0

Cram tests here. They run and compare program output to the expected output
https://dune.readthedocs.io/en/stable/tests.html#cram-tests
Use `dune promote` after you change things that should runned

If you need to put sample program and use it both in your interpreter and preinstalled one,
you could put it into separate file. Thise will need stanza `(cram (deps demo_input.txt))`
in the dune file

  $ ../bin/REPL.exe -dparsetree <<EOF
  > fun f -> x
  Parsed result: (Abs (f, (Var x)))
  Evaluated result: (fun _ -> x)
  $ ../bin/REPL.exe -dparsetree <<EOF
  > ( fun x -> fun x -> x ) 1 2
  Parsed result: (App ((App ((Abs (x, (Abs (x, (Var x))))), (Int 1))), (
                    Int 2)))
  Evaluated result: 2

Desugaring an abstraction
  $ ../bin/REPL.exe <<EOF
  > ( fun x y -> x + y ) 1 2
  Evaluated result: 3

This code should work after adding arithmetic operators
  $ ../bin/REPL.exe -dparsetree <<EOF
  > (fun x -> ((fun x -> x) 6) + x) 5
  Parsed result: (App (
                    (Abs (x,
                       (Binop (Plus, (App ((Abs (x, (Var x))), (Int 6))),
                          (Var x)))
                       )),
                    (Int 5)))
  Evaluated result: 11

CBV argument evaluation
  $ ../bin/REPL.exe -dparsetree <<EOF
  > (fun x -> x) ((fun  y -> y) 4)
  Parsed result: (App ((Abs (x, (Var x))), (App ((Abs (y, (Var y))), (Int 4)))
                    ))
  Evaluated result: 4

  $ ../bin/REPL.exe <<EOF
  > (fun x -> (fun y -> x + y)) 5 8
  Evaluated result: 13

  $ ../bin/REPL.exe <<EOF
  > (fun x -> x) ((fun y -> y * 2) 9)
  Evaluated result: 18

Integer arithmetic

  $ ../bin/REPL.exe <<EOF
  > 42
  Evaluated result: 42

  $ ../bin/REPL.exe <<EOF
  > 7 * 6
  Evaluated result: 42

  $ ../bin/REPL.exe <<EOF
  > 1 + 2 * 3
  Evaluated result: 7

  $ ../bin/REPL.exe -dparsetree <<EOF
  > (4 + 5*10 < (  4 + 5) * 10 - 28)
  Parsed result: (Binop (Lt,
                    (Binop (Plus, (Int 4), (Binop (Times, (Int 5), (Int 10))))),
                    (Binop (Minus,
                       (Binop (Times, (Binop (Plus, (Int 4), (Int 5))),
                          (Int 10))),
                       (Int 28)))
                    ))
  Evaluated result: 1

Negative numbers
  $ ../bin/REPL.exe <<EOF
  >  1 -2
  Evaluated result: -1

  $ ../bin/REPL.exe <<EOF
  >  - (1 -2)
  Evaluated result: 1

  $ ../bin/REPL.exe <<EOF
  >  -2
  Evaluated result: -2

Arithmetic and application precedence

  $ ../bin/REPL.exe <<EOF
  >  (fun x -> x) 4 + 5
  Evaluated result: 9

  $ ../bin/REPL.exe <<EOF
  >  (fun x -> x + 1) 2 * 3
  Evaluated result: 9

Condition
  $ ../bin/REPL.exe <<EOF
  >  if 1 then 2 else 3
  Evaluated result: 2

  $ ../bin/REPL.exe <<EOF
  >  if 0 then 2 else 3
  Evaluated result: 3

  $ ../bin/REPL.exe <<EOF
  >  if 1 + 1 then 2 * 3 else 3
  Evaluated result: 6

  $ ../bin/REPL.exe <<EOF
  >  if 1 then if 0 then 2 else 3 else 4
  Evaluated result: 3


  $ ../bin/REPL.exe <<EOF
  >  (if 1 then fun x -> x + 1 else fun x -> x + 2) 4
  Evaluated result: 5

  $ ../bin/REPL.exe <<EOF
  >  (if 0 then fun x -> x * 2 else fun x -> x * 3) 5
  Evaluated result: 15

  $ ../bin/REPL.exe <<EOF
  >  if (fun x -> x) 1 then 7 else 8
  Evaluated result: 7

  $ ../bin/REPL.exe <<EOF
  > (fun x -> if x then x + 1 else x - 1) 4
  Evaluated result: 5


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

