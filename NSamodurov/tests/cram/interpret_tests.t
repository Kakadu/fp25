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
  > let false = (fun x y -> y) in false 2 3
  Success: (Int 3)

  $ ../../bin/REPL.exe << EOF
  > let f = (fun x -> 5 + 1) in f 1
  Success: (Int 6)

