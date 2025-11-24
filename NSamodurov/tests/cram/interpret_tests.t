Copyright 2021-2024, Kakadu and contributors
SPDX-License-Identifier: CC0-1.0
  $ ../../bin/REPL.exe << EOF
  > 1
  Success: (Int 1)

  $ ../../bin/REPL.exe << EOF
  > (1 + 2)
  Success: (Int 3)

  $ ../../bin/REPL.exe << EOF
  > let x = 1 in x
  Success: (Int 1)

  $ ../../bin/REPL.exe << EOF
  > let f = fun x -> 5 + 1 in f 1
  Success: (Int 6)

  $ ../../bin/REPL.exe << EOF
  > let eq = 1 * 2 in eq
  Success: (Int 2)

  $ ../../bin/REPL.exe << EOF
  > if false then 123 else 321
  Success: (Int 321)

  $ ../../bin/REPL.exe << EOF
  > if 1 < 2 then 123 else 321
  Success: (Int 123)

  $ ../../bin/REPL.exe << EOF
  > let rec fact n = if n < 2 then 1 else n * fact (n-1) in fact 5
  Fatal error: exception Failure("Fix needed")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Lambda_lib__Interpret.interpret in file "lib/interpret.ml" (inlined), line 151, characters 2-25
  Called from Lambda_lib__Interpret.parse_and_run.helper in file "lib/interpret.ml", line 161, characters 8-25
  Called from Stdlib__Result.bind in file "result.ml" (inlined), line 23, characters 36-39
  Called from Lambda_lib__Interpret.parse_and_run.helper in file "lib/interpret.ml", lines 157-161, characters 4-25
  Called from Dune__exe__REPL in file "bin/REPL.ml", line 14, characters 2-20
  [2]

  $ ../../bin/REPL.exe << EOF
  > let rec fib n = if n <= 1 then n else fib (n-1) + fib(n-2) in
  > let rec fact n = if n < 2 then 1 else n * fact (n-1) in
  > fib 3
  Error: Can't interpret: Can't access a variable

  $ ../../bin/REPL.exe << EOF
  > let a = 3 in
  > let b = 2 in
  > let c = 1 in
  > c
  Success: (Int 1) 
