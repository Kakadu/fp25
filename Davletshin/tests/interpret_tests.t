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

This code should work after adding arithmetic operators
  $ ../bin/REPL.exe -dparsetree <<EOF
  > (fun x -> ((fun x -> x) 6) + x) 5
  Error: : count_while1

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

