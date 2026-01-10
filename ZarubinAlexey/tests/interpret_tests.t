Copyright 2021-2025, Kakadu and contributors
SPDX-License-Identifier: CC0-1.0

  $ ../bin/REPL.exe <<EOF
  > 5

  $ ../bin/REPL.exe <<EOF
  > let rec fact n = if n = 0 then 1 else n * fact (n - 1) in fact 5

  $ ../bin/REPL.exe <<EOF
  > let rec fib n =
  >   if n = 0 then 0
  >   else if n = 1 then 1
  >   else fib (n - 1) + fib (n - 2)
  > in fib 10

  $ ../bin/REPL.exe <<EOF
  > let rec infinite n = if n = 0 then infinite n else infinite n in infinite 5
