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
  > let r x = x+x*8 in r 9
  (let r = Fun (Var(x), (Var(x) + (Var(x) * CInt(8)))) in App (Var(r), CInt(9)))
  Fatal error: exception Failure("unimlemented")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Lambda_lib__Interpret.eval in file "lib/interpret.ml", line 133, characters 16-29
  Called from Lambda_lib__Interpret.run_interpret in file "lib/interpret.ml", line 189, characters 8-26
  Called from Dune__exe__REPL in file "bin/REPL.ml", line 20, characters 8-36
  [2]

  $ ../bin/REPL.exe <<EOF
  > (fun s k -> s+k) 5 7
  App (App (Fun (Var(s), Fun (Var(k), (Var(s) + Var(k)))), CInt(5)), CInt(7))
  CInt(12)

  $ ../bin/REPL.exe <<EOF
  > let r = (fun s k -> s+k) 5 7 in let p = (fun s->s*2) ((fun k -> k*3) 10) in p/2 + r
  (let r = App (App (Fun (Var(s), Fun (Var(k), (Var(s) + Var(k)))), CInt(5)), CInt(7)) in (let p = App (Fun (Var(s), (Var(s) * CInt(2))), App (Fun (Var(k), (Var(k) * CInt(3))), CInt(10))) in ((Var(p) / CInt(2)) + Var(r))))
  CInt(42)

