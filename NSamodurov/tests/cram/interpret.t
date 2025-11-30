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
  > let a = 3 in
  > let b = 2 in
  > let c = 1 in
  > c
  Success: (Int 1)

  $ ../../bin/REPL.exe << EOF
  > let rec fact acc n = if n <= 1 then acc else fact (n * acc) (n-1) in
  > fact 1 4
  Success: (Int 24)

  $ ../../bin/REPL.exe << EOF
  > let rec fact n = if n <= 1 then n else n * fact (n-1) in
  > fact 10
  Success: (Int 3628800)

  $ ../../bin/REPL.exe << EOF
  > let id x = x in
  > let idd x = x in
  > (idd (12 - 1) + id (23 - 1))
  Success: (Int 33)

  $ ../../bin/REPL.exe << EOF
  > let rec fib n =
  > if n <= 1 then
  >   n
  > else
  >   fib (n-1) + fib (n-2)
  > in
  > fib 12
  Success: (Int 144)

  $ ../../bin/REPL.exe << EOF
  > let rec fib n =
  > if n <= 1 then
  >   n
  > else
  >   fib (n-1) + fib (n-2)
  > in
  > print (fib 12)
  144
  Success: (Int 0)
