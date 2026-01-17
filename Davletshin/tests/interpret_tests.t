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
  Evaluated result: (fun _ -> x)
  $ ../bin/REPL.exe -dparsetree <<EOF
  > garbage242
  Parsed result: (Var garbage242)
  Evaluated result: garbage242



  $ ../bin/REPL.exe -no -dparsetree <<EOF
  > (fun x -> fun y -> x)(fun u -> u)((fun x -> x x)(fun x -> x x))
  Parsed result: (App (
                    (App ((Abs (x, (Abs (y, (Var x))))), (Abs (u, (Var u))))),
                    (App ((Abs (x, (App ((Var x), (Var x))))),
                       (Abs (x, (App ((Var x), (Var x)))))))
                    ))
  Evaluated result: (fun u -> u)
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
  Evaluated result: (fun n f x _x -> ((f (n (f x))) _x))
  $ ../bin/REPL.exe -ao   < lam_2x1.txt
  Evaluated result: 2
Call by value doesn't reduce under abstraction
  $ ../bin/REPL.exe -cbv   < lam_2x1.txt
  Evaluated result: (fun z -> (2 (1 z)))
  $ ../bin/REPL.exe -ao -small   < lam_3x2.txt
   -- ((fun y z -> ((fun f x -> (f (f (f x)))) (y z))) 2)
   -- ((fun y z x -> ((y z) ((y z) ((y z) x)))) 2)
   -- (fun z x -> ((2 z) ((2 z) ((2 z) x))))
   -- (fun z x -> ((fun x -> (z (z x))) ((2 z) ((2 z) x))))
   -- (fun z x -> ((fun x -> (z (z x))) ((fun x -> (z (z x))) ((2 z) x))))
   -- (fun z x -> ((fun x -> (z (z x))) ((fun x -> (z (z x))) ((fun x -> (z (z x))) x))))
   -- (fun z x -> ((fun x -> (z (z x))) ((fun x -> (z (z x))) (z (z x)))))
   -- (fun z x -> ((fun x -> (z (z x))) (z (z (z (z x))))))
   -- (fun z x -> (z (z (z (z (z (z x)))))))
  Evaluated result: (fun z x -> (z (z (z (z (z (z x)))))))
  $ ../bin/REPL.exe -ao   < lam_zero.txt
  Evaluated result: ⊥
For 3! we use noral order reduction
  $ cat lam_fac3.txt
  (((fun f -> ((fun x -> (f (x x))) (fun x -> (f (x x))))) (fun s -> (fun n -> ((((fun n -> ((n (fun x -> (fun x -> (fun y -> y)))) (fun x -> (fun y -> x)))) n) (fun f -> (fun x -> (f x)))) (((fun x -> (fun y -> (fun z -> (x (y z))))) (s ((fun n -> (fun f -> (fun x -> (((n (fun g -> (fun h -> (h (g f))))) (fun u -> x)) (fun u -> u))))) n))) n))))) (fun f -> (fun x -> (f (f (f x))))))
  $ ../bin/REPL.exe -no   < lam_fac3.txt
  Evaluated result: (fun z x -> (z (z (z (z (z (z x)))))))
