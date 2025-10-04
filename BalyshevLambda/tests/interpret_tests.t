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
  Evaluated!
  Result: (λ _ . x)

  $ ../bin/REPL.exe -dparsetree <<EOF
  > garbage242
  Error: : end_of_input



  $ ../bin/REPL.exe -no -dparsetree <<EOF
  > (\x.\y.x)(\u.u)((\x. x x)(\x.x x))
  Parsed result: (App (
                    (App ((Abs (x, (Abs (y, (Var x))))), (Abs (u, (Var u))))),
                    (App ((Abs (x, (App ((Var x), (Var x))))),
                       (Abs (x, (App ((Var x), (Var x)))))))
                    ))
  Evaluated!
  Result: (λ u . u)
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
  Evaluated!
  Result: 1
  $ ../bin/REPL.exe -ao   < lam_2x1.txt
  Evaluated!
  Result: 1
Call by value doesn't reduce under abstraction
  $ ../bin/REPL.exe -cbv   < lam_2x1.txt
  Evaluated!
  Result: 1
  $ ../bin/REPL.exe -ao -small   < lam_3x2.txt
  Evaluated!
  Result: 2
  $ ../bin/REPL.exe -ao   < lam_zero.txt
  Evaluated!
  Result: ⊥
For 3! we use noral order reduction
  $ cat lam_fac3.txt
  (((λ f . ((λ x . (f (x x))) (λ x . (f (x x))))) (λ s . (λ n . ((((λ n . ((n (λ x . (λ x . (λ y . y)))) (λ x . (λ y . x)))) n) (λ f . (λ x . (f x)))) (((λ x . (λ y . (λ z . (x (y z))))) (s ((λ n . (λ f . (λ x . (((n (λ g . (λ h . (h (g f))))) (λ u . x)) (λ u . u))))) n))) n))))) (λ f . (λ x . (f (f (f x))))))
  $ ../bin/REPL.exe -no   < lam_fac3.txt
  Evaluated!
  Result: (λ z x -> (z (z (z (z (z (z x)))))))

tests for limited reductions
cbn under abstraction
  $ ../bin/REPL.exe -cbn -lim 1 <<EOF
  > \x. (\y.y) z
  Evaluated! Reductions left: 1.
  Result: (λ _ . ((λ y . y) z))

cbn on application
  $ ../bin/REPL.exe -cbn -lim 2 <<EOF
  > (\x.f x) ((\y.y) a)
  Evaluated! Reductions left: 1.
  Result: (f ((λ y . y) a))

cbv under abstraction
  $ ../bin/REPL.exe -cbv -lim 1 <<EOF
  > \x. (\y.y) z
  Evaluated! Reductions left: 1.
  Result: (λ _ . ((λ y . y) z))

cbv on application
  $ ../bin/REPL.exe -cbv -lim 1 <<EOF
  > (\x.x) ((\y.y) z)
  Partial evaluated.
  Result: ((λ x . x) z)

ao under abstraction
  $ ../bin/REPL.exe -ao -lim 1 <<EOF
  > \x. (\y.y) z
  Evaluated! Reductions left: 0.
  Result: (λ _ . z)

ao on application
  $ ../bin/REPL.exe -cbv -lim 1 <<EOF
  > (\x.x) ((\y.y) z)
  Partial evaluated.
  Result: ((λ x . x) z)

  $ ../bin/REPL.exe -ao -lim 2 <<EOF
  > (\x.f x) ((\y.y) a)
  Evaluated! Reductions left: 0.
  Result: (f a)

  $ ../bin/REPL.exe -ao -lim 1 <<EOF
  > \x. (\y.y) z
  Evaluated! Reductions left: 0.
  Result: (λ _ . z)

  $ ../bin/REPL.exe -ao -lim 1 <<EOF
  > f ((\x.x) y)
  Evaluated! Reductions left: 0.
  Result: (f y)

cbn on Omega combinator
  $ ../bin/REPL.exe -cbn -lim 10 <<EOF
  > (\x.x x) (\y.y y)
  Partial evaluated.
  Result: ((λ y . (y y)) (λ y . (y y)))

various strategies of frst which ignores Omega
  $ ../bin/REPL.exe -cbn -lim 10 <<EOF
  > (\x.\y.x) a ((\x.x x) (\y.y y))
  Evaluated! Reductions left: 8.
  Result: a

  $ ../bin/REPL.exe -cbv -lim 10 <<EOF
  > (\x.\y.x) a ((\x.x x) (\y.y y))
  Partial evaluated.
  Result: ((λ _ . a) ((λ y . (y y)) (λ y . (y y))))

  $ ../bin/REPL.exe -ao -lim 10 <<EOF
  > (\x.\y.x) a ((\x.x x) (\y.y y))
  Partial evaluated.
  Result: ((λ _ . a) ((λ y . (y y)) (λ y . (y y))))
