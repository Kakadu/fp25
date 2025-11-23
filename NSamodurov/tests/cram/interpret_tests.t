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
  > let rec id x = if (true) then 0 else id (x-1) in id 5
  Error: Can't interpret: Operand is not int
