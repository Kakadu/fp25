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
  (let _ = CInt(8))
  Fatal error: exception Failure("unimlemented")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Lambda_lib__Interpret.run_interpret in file "lib/interpret.ml", line 127, characters 8-26
  Called from Dune__exe__REPL in file "bin/REPL.ml", line 19, characters 8-36
  [2]
  $ ../bin/REPL.exe -dparsetree <<EOF
  > garbage242
  (let _ = CInt(8))
  Fatal error: exception Failure("unimlemented")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Lambda_lib__Interpret.run_interpret in file "lib/interpret.ml", line 127, characters 8-26
  Called from Dune__exe__REPL in file "bin/REPL.ml", line 19, characters 8-36
  [2]



  $ ../bin/REPL.exe -no -dparsetree <<EOF
  > (\x.\y.x)(\u.u)((\x. x x)(\x.x x))
  (let _ = CInt(8))
  Fatal error: exception Failure("unimlemented")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Lambda_lib__Interpret.run_interpret in file "lib/interpret.ml", line 127, characters 8-26
  Called from Dune__exe__REPL in file "bin/REPL.ml", line 19, characters 8-36
  [2]
Below we redirect contents of the file to the evaluator
  $ ../bin/REPL.exe -dparsetree -stop-after parsing   < lam_1+1.txt
  cannot open lam_1+1.txt: No such file
  [2]

  $ ../bin/REPL.exe -ao   < lam_1+1.txt
  cannot open lam_1+1.txt: No such file
  [2]
  $ ../bin/REPL.exe -ao   < lam_2x1.txt
  cannot open lam_2x1.txt: No such file
  [2]
Call by value doesn't reduce under abstraction
  $ ../bin/REPL.exe -cbv   < lam_2x1.txt
  cannot open lam_2x1.txt: No such file
  [2]
  $ ../bin/REPL.exe -ao -small   < lam_3x2.txt
  cannot open lam_3x2.txt: No such file
  [2]
  $ ../bin/REPL.exe -ao   < lam_zero.txt
  cannot open lam_zero.txt: No such file
  [2]
For 3! we use noral order reduction
  $ cat lam_fac3.txt
  cat: lam_fac3.txt: No such file or directory
  [1]
  $ ../bin/REPL.exe -no   < lam_fac3.txt
  cannot open lam_fac3.txt: No such file
  [2]
