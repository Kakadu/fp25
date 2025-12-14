Copyright 2021-2024, Kakadu and contributors
SPDX-License-Identifier: CC0-1.0

Cram tests here. They run and compare program output to the expected output
https://dune.readthedocs.io/en/stable/tests.html#cram-tests
Use `dune promote` after you change things that should runned

If you need to put sample program and use it both in your interpreter and preinstalled one,
you could put it into separate file. Thise will need stanza `(cram (deps demo_input.txt))`
in the dune file

  $ ../bin/REPL.exe <<EOF
  > 5
  Int 5
  5
  

  $ ../bin/REPL.exe <<EOF
  > x
  Var "x"
  Unbound variable: x
  
  $ ../bin/REPL.exe << EOF
  > let h = 1 + 1 in h 
  Let (NonRec, "h", BinOp (Plus, Int 1, Int 1), Some Var "h")
  2
  


