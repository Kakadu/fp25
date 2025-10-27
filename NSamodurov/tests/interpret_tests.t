Copyright 2021-2024, Kakadu and contributors
SPDX-License-Identifier: CC0-1.0

Cram tests here. They run and compare program output to the expected output
https://dune.readthedocs.io/en/stable/tests.html#cram-tests
Use `dune promote` after you change things that should runned

If you need to put sample program and use it both in your interpreter and preinstalled one,
you could put it into separate file. Thise will need stanza `(cram (deps demo_input.txt))`
in the dune file

  $ ../bin/REPL.exe -cbv -dparsetree <<EOF
  > fun f -> x
  Parsed result: (Abs (f, (Var x)))
  99 beta-reductions left
  Evaluated result: (λ _ . x)
  $ ../bin/REPL.exe -dparsetree <<EOF
  > garbage242
  Parsed result: (Var garbage242)
  99 beta-reductions left
  Evaluated result: garbage242

  $ ../bin/REPL.exe -no -dparsetree <<EOF
  > (fun x -> fun y -> x)(fun u -> u)((fun x ->  x x)(fun x -> x x))
  Parsed result: (App (
                    (App ((Abs (x, (Abs (y, (Var x))))), (Abs (u, (Var u))))),
                    (App ((Abs (x, (App ((Var x), (Var x))))),
                       (Abs (x, (App ((Var x), (Var x)))))))
                    ))
  97 beta-reductions left
  Evaluated result: (λ u . u)

  $ ../bin/REPL.exe -no -dparsetree <<EOF
  > (fun x -> x x)(fun x -> x x)
  Parsed result: (App ((Abs (x, (App ((Var x), (Var x))))),
                    (Abs (x, (App ((Var x), (Var x)))))))
  Evaluated result: ((λ x . (x x)) (λ x . (x x)))
  Number of beta-reduction has reached a limit

  $ ../bin/REPL.exe -no -dparsetree <<EOF
  > (fun x y z  -> x x)
  Parsed result: (Abs (x, (Abs (y, (Abs (z, (App ((Var x), (Var x)))))))))
  97 beta-reductions left
  Evaluated result: (λ x _ _ -> (x x))
