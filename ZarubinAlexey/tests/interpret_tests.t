Copyright 2021-2025, Kakadu and contributors
SPDX-License-Identifier: CC0-1.0

  $ ../bin/REPL.exe <<'EOF'
  > 5
  > EOF
  5

  $ ../bin/REPL.exe <<'EOF'
  > 0
  > EOF
  0

  $ ../bin/REPL.exe <<'EOF'
  > 2 + 3 * 4
  > EOF
  14

  $ ../bin/REPL.exe <<'EOF'
  > (2 + 3) * 4
  > EOF
  20

  $ ../bin/REPL.exe <<'EOF'
  > 10 - 3 - 2
  > EOF
  5

  $ ../bin/REPL.exe <<'EOF'
  > 8 / 2 / 2
  > EOF
  2

  $ ../bin/REPL.exe <<'EOF'
  > -5 + 2
  > EOF
  -3

  $ ../bin/REPL.exe <<'EOF'
  > -(2 + 3) * 4
  > EOF
  -20

  $ ../bin/REPL.exe <<'EOF'
  > let x = 10 in x + 7
  > EOF
  17

  $ ../bin/REPL.exe <<'EOF'
  > let x = 1 in let x = 2 in x + 10
  > EOF
  12

  $ ../bin/REPL.exe <<'EOF'
  > let x = 3 in let y = 4 in x * y + 1
  > EOF
  13

  $ ../bin/REPL.exe <<'EOF'
  > (fun x -> x + 1) 5
  > EOF
  6

  $ ../bin/REPL.exe <<'EOF'
  > let add = fun x y -> x + y in add 3 4
  > EOF
  7

  $ ../bin/REPL.exe <<'EOF'
  > let inc = fun x -> x + 1 in inc (inc 0)
  > EOF
  2

  $ ../bin/REPL.exe <<'EOF'
  > let x = 10 in
  > let f = fun y -> x + y in
  > let x = 100 in
  > f 5
  > EOF
  15

  $ ../bin/REPL.exe <<'EOF'
  > if 0 then 111 else 222
  > EOF
  222

  $ ../bin/REPL.exe <<'EOF'
  > if 1 then 111 else 222
  > EOF
  111

  $ ../bin/REPL.exe <<'EOF'
  > if 2 = 2 then 1 else 0
  > EOF
  1

  $ ../bin/REPL.exe <<'EOF'
  > if 2 < 2 then 1 else 0
  > EOF
  0

  $ ../bin/REPL.exe <<'EOF'
  > if 3 > 2 then 1 else 0
  > EOF
  1

  $ ../bin/REPL.exe <<'EOF'
  > let rec fact n = if n = 0 then 1 else n * fact (n - 1) in fact 5
  > EOF
  120

  $ ../bin/REPL.exe <<'EOF'
  > let rec fib n =
  >   if n = 0 then 0
  >   else if n = 1 then 1
  >   else fib (n - 1) + fib (n - 2)
  > in fib 10
  > EOF
  55

  $ ../bin/REPL.exe <<'EOF'
  > let _ = print 10 in 5
  > EOF
  10
  5

  $ ../bin/REPL.exe <<'EOF'
  > let _ = print 1 in let _ = print 2 in 3
  > EOF
  1
  2
  3

  $ ../bin/REPL.exe <<'EOF'
  > let _ = print (2 + 3 * 4) in 0
  > EOF
  14
  0

  $ ../bin/REPL.exe <<'EOF' [1]
  > let x = in 5
  > EOF
  Parsing error
  [1]

  $ ../bin/REPL.exe 2>&1 <<'EOF' [1]
  > x + 1
  > EOF
  Interpreter error: unknown variable x
  [1]

  $ ../bin/REPL.exe 2>&1 <<'EOF' [1]
  > (5) 3
  > EOF
  Interpreter error: not a function
  [1]

  $ ../bin/REPL.exe 2>&1 <<'EOF' [1]
  > 10 / 0
  > EOF
  Interpreter error: division by zero
  [1]

  $ ../bin/REPL.exe 2>&1 <<'EOF' [1]
  > fun x -> x
  > EOF
  Interpreter error: result is not int
  [1]

  $ ../bin/REPL.exe 2>&1 <<'EOF' [1]
  > 1 + (fun x -> x)
  > EOF
  Interpreter error: binop on non-int
  [1]

  $ ../bin/REPL.exe 2>&1 <<'EOF' [1]
  > if fun x -> x then 1 else 2
  > EOF
  Interpreter error: if condition is not int
  [1]

  $ ../bin/REPL.exe 2>&1 <<'EOF' [1]
  > print (fun x -> x)
  > EOF
  Interpreter error: print argument not int
  [1]
