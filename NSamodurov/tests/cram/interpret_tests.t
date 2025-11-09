Copyright 2021-2024, Kakadu and contributors
SPDX-License-Identifier: CC0-1.0
  $ ../../bin/REPL.exe -cbv -dparsetree <<EOF
  > fun f -> x
  Parsed result: (EAbs (f, (EVar x)))
  99 beta-reductions left
  Evaluated result: (λ _ . x)

  $ ../../bin/REPL.exe -dparsetree <<EOF
  > garbage242
  Parsed result: (EVar garbage242)
  99 beta-reductions left
  Evaluated result: garbage242

  $ ../../bin/REPL.exe -no -dparsetree <<EOF
  > (fun x -> fun y -> x)(fun u -> u)((fun x ->  x x)(fun x -> x x))
  Parsed result: (EApp (
                    (EApp ((EAbs (x, (EAbs (y, (EVar x))))),
                       (EAbs (u, (EVar u))))),
                    (EApp ((EAbs (x, (EApp ((EVar x), (EVar x))))),
                       (EAbs (x, (EApp ((EVar x), (EVar x)))))))
                    ))
  97 beta-reductions left
  Evaluated result: (λ u . u)

  $ ../../bin/REPL.exe -no -dparsetree <<EOF
  > (fun x -> x x)(fun x -> x x)
  Parsed result: (EApp ((EAbs (x, (EApp ((EVar x), (EVar x))))),
                    (EAbs (x, (EApp ((EVar x), (EVar x)))))))
  Evaluated result: ((λ x . (x x)) (λ x . (x x)))
  Number of beta-reduction has reached a limit

  $ ../../bin/REPL.exe -no -dparsetree <<EOF
  > (fun x y z  -> x x)
  Parsed result: (EAbs (x, (EAbs (y, (EAbs (z, (EApp ((EVar x), (EVar x)))))))
                    ))
  97 beta-reductions left
  Evaluated result: (λ x _ _ -> (x x))
