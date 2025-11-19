Copyright 2021-2024, Kakadu and contributors
SPDX-License-Identifier: CC0-1.0

Cram tests here. They run and compare program output to the expected output
https://dune.readthedocs.io/en/stable/tests.html#cram-tests
Use `dune promote` after you change things that should runned

If you need to put sample program and use it both in your interpreter and preinstalled one,
you could put it into separate file. Thise will need stanza `(cram (deps demo_input.txt))`
in the dune file

  $ ../bin/REPL.exe <<EOF
  > 5 + 5
  (CInt(5) + CInt(5))
  CInt(10)
  $ ../bin/REPL.exe -dparsetree <<EOF
  > let r x y= y+x*8 in r 9 
  (let r = Fun (Var(x), Fun (Var(y), (Var(y) + (Var(x) * CInt(8))))) in App (Var(r), CInt(9)))
  Fun (Var(y), (Var(y) + (CInt(9) * CInt(8))))

  $ ../bin/REPL.exe <<EOF
  > (fun s k -> s+k) 5 7
  App (App (Fun (Var(s), Fun (Var(k), (Var(s) + Var(k)))), CInt(5)), CInt(7))
  CInt(12)

  $ ../bin/REPL.exe <<EOF
  > let r = (fun s k -> s+k) 5 7 in let p = (fun s->s*2) ((fun k -> k*3) 10) in p/2 + r
  (let r = App (App (Fun (Var(s), Fun (Var(k), (Var(s) + Var(k)))), CInt(5)), CInt(7)) in (let p = App (Fun (Var(s), (Var(s) * CInt(2))), App (Fun (Var(k), (Var(k) * CInt(3))), CInt(10))) in ((Var(p) / CInt(2)) + Var(r))))
  CInt(42)

  $ ../bin/REPL.exe <<EOF
  > let x = 7*8+9 in (fun x -> x+x) 5
  (let x = ((CInt(7) * CInt(8)) + CInt(9)) in App (Fun (Var(x), (Var(x) + Var(x))), CInt(5)))
  CInt(10)

  $ ../bin/REPL.exe <<EOF
  > let x = 7 in let function a b = if x > 4 then x+b else a-b in function 0 1
  (let x = CInt(7) in (let function = Fun (Var(a), Fun (Var(b), if ((Var(x) > CInt(4))) then ((Var(x) + Var(b))) else ((Var(a) - Var(b))))) in App (App (Var(function), CInt(0)), CInt(1))))
  CInt(8)
