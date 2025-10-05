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
  Result: (λ f . x)

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
  Result: (λ n . (λ f . (λ x . (λ _x . ((f (n (f x))) _x)))))
  $ ../bin/REPL.exe -ao   < lam_2x1.txt
  Evaluated!
  Result: (λ z . (λ x . (z (z x))))
Call by value doesn't reduce under abstraction
  $ ../bin/REPL.exe -cbv   < lam_2x1.txt
  Evaluated!
  Result: (λ z . ((λ f . (λ x . (f (f x)))) ((λ f . (λ x . (f x))) z)))
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
  Evaluated!
  Result: (λ z . (λ x . (z (z (z (z (z (z x))))))))
  $ ../bin/REPL.exe -ao   < lam_zero.txt
  Evaluated!
  Result: (λ g . (λ y . y))
For 3! we use noral order reduction
  $ cat lam_fac3.txt
  (((λ f . ((λ x . (f (x x))) (λ x . (f (x x))))) (λ s . (λ n . ((((λ n . ((n (λ x . (λ x . (λ y . y)))) (λ x . (λ y . x)))) n) (λ f . (λ x . (f x)))) (((λ x . (λ y . (λ z . (x (y z))))) (s ((λ n . (λ f . (λ x . (((n (λ g . (λ h . (h (g f))))) (λ u . x)) (λ u . u))))) n))) n))))) (λ f . (λ x . (f (f (f x))))))
  $ ../bin/REPL.exe -no   < lam_fac3.txt
  Evaluated!
  Result: (λ z . (λ x . (z (z (z (z (z (z x))))))))

tests for limited reductions

various strategies under abstraction
  $ ../bin/REPL.exe -cbn -lim 1 <<EOF
  > \x. (\y.y) z
  Evaluated! Reductions left: 1.
  Result: (λ x . ((λ y . y) z))

  $ ../bin/REPL.exe -cbv -lim 1 <<EOF
  > \x. (\y.y) z
  Evaluated! Reductions left: 1.
  Result: (λ x . ((λ y . y) z))

  $ ../bin/REPL.exe -ao -lim 1 <<EOF
  > \x. (\y.y) z
  Evaluated! Reductions left: 0.
  Result: (λ x . z)

  $ ../bin/REPL.exe -no -lim 1 <<EOF
  > \x. (\y.y) z
  Evaluated! Reductions left: 0.
  Result: (λ x . z)

various strategies on redex as application argument
  $ ../bin/REPL.exe -cbn -lim 1 <<EOF
  > (\x.x) ((\y.y) z)
  Partial evaluated.
  Result: ((λ y . y) z)

  $ ../bin/REPL.exe -cbv -lim 1 <<EOF
  > (\x.x) ((\y.y) z)
  Partial evaluated.
  Result: ((λ x . x) z)

  $ ../bin/REPL.exe -ao -lim 1 <<EOF
  > (\x.x) ((\y.y) z)
  Partial evaluated.
  Result: ((λ x . x) z)

  $ ../bin/REPL.exe -no -lim 1 <<EOF
  > (\x.x) ((\y.y) z)
  Partial evaluated.
  Result: ((λ y . y) z)

  $ ../bin/REPL.exe -cbn -lim 1 <<EOF
  > f ((\x.x) y)
  Evaluated! Reductions left: 1.
  Result: (f ((λ x . x) y))

  $ ../bin/REPL.exe -cbv -lim 1 <<EOF
  > f ((\x.x) y)
  Evaluated! Reductions left: 0.
  Result: (f y)

  $ ../bin/REPL.exe -ao -lim 1 <<EOF
  > f ((\x.x) y)
  Evaluated! Reductions left: 0.
  Result: (f y)

  $ ../bin/REPL.exe -no -lim 1 <<EOF
  > f ((\x.x) y)
  Evaluated! Reductions left: 0.
  Result: (f y)

various strategies of frst which ignores Omega combinator
  $ ../bin/REPL.exe -cbn -lim 10 <<EOF
  > (\x.\y.x) a ((\x.x x) (\y.y y))
  Evaluated! Reductions left: 8.
  Result: a

  $ ../bin/REPL.exe -cbv -lim 10 <<EOF
  > (\x.\y.x) a ((\x.x x) (\y.y y))
  Partial evaluated.
  Result: ((λ y . a) ((λ y . (y y)) (λ y . (y y))))

  $ ../bin/REPL.exe -ao -lim 10 <<EOF
  > (\x.\y.x) a ((\x.x x) (\y.y y))
  Partial evaluated.
  Result: ((λ y . a) ((λ y . (y y)) (λ y . (y y))))

  $ ../bin/REPL.exe -no -lim 10 <<EOF
  > (\x.\y.x) a ((\x.x x) (\y.y y))
  Evaluated! Reductions left: 8.
  Result: a

lam_*.txt
  $ ../bin/REPL.exe -cbn   < lam_2x1.txt
  Evaluated!
  Result: (λ z . ((λ f . (λ x . (f (f x)))) ((λ f . (λ x . (f x))) z)))

  $ ../bin/REPL.exe -cbv   < lam_2x1.txt
  Evaluated!
  Result: (λ z . ((λ f . (λ x . (f (f x)))) ((λ f . (λ x . (f x))) z)))

  $ ../bin/REPL.exe -ao   < lam_2x1.txt
  Evaluated!
  Result: (λ z . (λ x . (z (z x))))

  $ ../bin/REPL.exe -no   < lam_2x1.txt
  Evaluated!
  Result: (λ z . (λ x . (z (z x))))

  $ ../bin/REPL.exe -cbn   < lam_3x2.txt
  Evaluated!
  Result: (λ z . ((λ f . (λ x . (f (f (f x))))) ((λ f . (λ x . (f (f x)))) z)))

  $ ../bin/REPL.exe -cbv   < lam_3x2.txt
  Evaluated!
  Result: (λ z . ((λ f . (λ x . (f (f (f x))))) ((λ f . (λ x . (f (f x)))) z)))

  $ ../bin/REPL.exe -ao   < lam_3x2.txt
  Evaluated!
  Result: (λ z . (λ x . (z (z (z (z (z (z x))))))))

  $ ../bin/REPL.exe -no   < lam_3x2.txt
  Evaluated!
  Result: (λ z . (λ x . (z (z (z (z (z (z x))))))))

  $ ../bin/REPL.exe -no -lim 200 < lam_fac3.txt
  Evaluated! Reductions left: 63.
  Result: (λ z . (λ x . (z (z (z (z (z (z x))))))))

  $ ../bin/REPL.exe -ao -lim 1000 < lam_fac3.txt
  Partial evaluated.
  Result: (((λ f . (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f ((λ x . (f (x x))) (λ x . (f (x x))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (λ s . (λ n . ((((λ n . ((n (λ x . (λ x . (λ y . y)))) (λ x . (λ y . x)))) n) (λ f . (λ x . (f x)))) (((λ x . (λ y . (λ z . (x (y z))))) (s ((λ n . (λ f . (λ x . (((n (λ g . (λ h . (h (g f))))) (λ u . x)) (λ u . u))))) n))) n))))) (λ f . (λ x . (f (f (f x))))))

  $ ../bin/REPL.exe -no -lim 2000 < lam_fac6.txt
  Evaluated! Reductions left: 568.
  Result: (λ z . (λ x . (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z x))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

  $ ../bin/REPL.exe -no -lim 200 < lam_3x6.txt
  Evaluated! Reductions left: 191.
  Result: (λ z . (λ x . (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z x))))))))))))))))))))

  $ ../bin/REPL.exe -ao -lim 200 < lam_3x6.txt
  Evaluated! Reductions left: 191.
  Result: (λ z . (λ x . (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z x))))))))))))))))))))

some arithmetics

(2 + 1) * 2
  $ ../bin/REPL.exe -ao <<EOF
  > (λm.λn.λf.m (n f)) ((λm.λn.λf.λx.m f (n f x)) (λf.λx.f (f x)) (λf.λx.f x)) (λf.λx.f (f x))
  Evaluated!
  Result: (λ f . (λ x . (f (f (f (f (f (f x))))))))

  $ ../bin/REPL.exe -no <<EOF
  > (λm.λn.λf.m (n f)) ((λm.λn.λf.λx.m f (n f x)) (λf.λx.f (f x)) (λf.λx.f x)) (λf.λx.f (f x))
  Evaluated!
  Result: (λ f . (λ x . (f (f (f (f (f (f x))))))))

(3 + 2) * (1 + 2) - 1 
  $ ../bin/REPL.exe -ao <<EOF
  > (λm.λn.n (λn.λf.λx.n (λg.λh.h (g f)) (λu.x) (λu.u)) m) ((λm.λn.λf.m (n f)) ((λm.λn.λf.λx.m f (n f x)) (λf.λx.f (f (f x))) (λf.λx.f (f x))) ((λm.λn.λf.λx.m f (n f x)) (λf.λx.f x) (λf.λx.f (f x)))) (λf.λx.f x)
  Evaluated!
  Result: (λ f . (λ _x . (f (f (f (f (f (f (f (f (f (f (f (f (f (f _x))))))))))))))))

  $ ../bin/REPL.exe -no <<EOF
  > (λm.λn.n (λn.λf.λx.n (λg.λh.h (g f)) (λu.x) (λu.u)) m) ((λm.λn.λf.m (n f)) ((λm.λn.λf.λx.m f (n f x)) (λf.λx.f (f (f x))) (λf.λx.f (f x))) ((λm.λn.λf.λx.m f (n f x)) (λf.λx.f x) (λf.λx.f (f x)))) (λf.λx.f x)
  Evaluated!
  Result: (λ f . (λ x . (f (f (f (f (f (f (f (f (f (f (f (f (f (f x))))))))))))))))

small step evaluation
  $ ../bin/REPL.exe -ao -small < lam_3x2.txt
   -- ((λ y z -> ((λ f x -> (f (f (f x)))) (y z))) 2)
   -- ((λ y z x -> ((y z) ((y z) ((y z) x)))) 2)
   -- (λ z x -> ((2 z) ((2 z) ((2 z) x))))
   -- (λ z x -> ((λ x . (z (z x))) ((2 z) ((2 z) x))))
   -- (λ z x -> ((λ x . (z (z x))) ((λ x . (z (z x))) ((2 z) x))))
   -- (λ z x -> ((λ x . (z (z x))) ((λ x . (z (z x))) ((λ x . (z (z x))) x))))
   -- (λ z x -> ((λ x . (z (z x))) ((λ x . (z (z x))) (z (z x)))))
   -- (λ z x -> ((λ x . (z (z x))) (z (z (z (z x))))))
   -- (λ z x -> (z (z (z (z (z (z x)))))))
  Evaluated!
  Result: (λ z . (λ x . (z (z (z (z (z (z x))))))))

  $ ../bin/REPL.exe -no -small < lam_3x2.txt
   -- ((λ y z -> ((λ f x -> (f (f (f x)))) (y z))) 2)
   -- (λ z . ((λ f x -> (f (f (f x)))) (2 z)))
   -- (λ z x -> ((2 z) ((2 z) ((2 z) x))))
   -- (λ z x -> ((λ x . (z (z x))) ((2 z) ((2 z) x))))
   -- (λ z x -> (z (z ((2 z) ((2 z) x)))))
   -- (λ z x -> (z (z ((λ x . (z (z x))) ((2 z) x)))))
   -- (λ z x -> (z (z (z (z ((2 z) x))))))
   -- (λ z x -> (z (z (z (z ((λ x . (z (z x))) x))))))
   -- (λ z x -> (z (z (z (z (z (z x)))))))
  Evaluated!
  Result: (λ z . (λ x . (z (z (z (z (z (z x))))))))

  $ ../bin/REPL.exe -ao -small < lam_3x6.txt
   -- ((λ y z -> ((λ f x -> (f (f (f x)))) (y z))) (λ f x -> (f (f (f (f (f (f x))))))))
   -- ((λ y z x -> ((y z) ((y z) ((y z) x)))) (λ f x -> (f (f (f (f (f (f x))))))))
   -- (λ z x -> (((λ f x -> (f (f (f (f (f (f x))))))) z) (((λ f x -> (f (f (f (f (f (f x))))))) z) (((λ f x -> (f (f (f (f (f (f x))))))) z) x))))
   -- (λ z x -> ((λ x . (z (z (z (z (z (z x))))))) (((λ f x -> (f (f (f (f (f (f x))))))) z) (((λ f x -> (f (f (f (f (f (f x))))))) z) x))))
   -- (λ z x -> ((λ x . (z (z (z (z (z (z x))))))) ((λ x . (z (z (z (z (z (z x))))))) (((λ f x -> (f (f (f (f (f (f x))))))) z) x))))
   -- (λ z x -> ((λ x . (z (z (z (z (z (z x))))))) ((λ x . (z (z (z (z (z (z x))))))) ((λ x . (z (z (z (z (z (z x))))))) x))))
   -- (λ z x -> ((λ x . (z (z (z (z (z (z x))))))) ((λ x . (z (z (z (z (z (z x))))))) (z (z (z (z (z (z x)))))))))
   -- (λ z x -> ((λ x . (z (z (z (z (z (z x))))))) (z (z (z (z (z (z (z (z (z (z (z (z x))))))))))))))
   -- (λ z x -> (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z x)))))))))))))))))))
  Evaluated!
  Result: (λ z . (λ x . (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z (z x))))))))))))))))))))
