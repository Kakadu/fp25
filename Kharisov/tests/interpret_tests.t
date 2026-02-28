Copyright 2021-2024, Kakadu and contributors
SPDX-License-Identifier: CC0-1.0

  $ ../bin/REPL.exe <<'EOF'
  > 42
  > EOF
  42

  $ ../bin/REPL.exe <<'EOF'
  > 0
  > EOF
  0

  $ ../bin/REPL.exe <<'EOF'
  > 2 + 3
  > EOF
  5

  $ ../bin/REPL.exe <<'EOF'
  > 10 - 4
  > EOF
  6

  $ ../bin/REPL.exe <<'EOF'
  > 6 * 7
  > EOF
  42

  $ ../bin/REPL.exe <<'EOF'
  > 15 / 3
  > EOF
  5

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
  > -5 + 3
  > EOF
  -2

  $ ../bin/REPL.exe <<'EOF'
  > -(2 + 3) * 4
  > EOF
  -20

  $ ../bin/REPL.exe <<'EOF'
  > 5 = 5
  > EOF
  1

  $ ../bin/REPL.exe <<'EOF'
  > 5 <> 3
  > EOF
  1

  $ ../bin/REPL.exe <<'EOF'
  > 3 < 5
  > EOF
  1

  $ ../bin/REPL.exe <<'EOF'
  > 5 <= 5
  > EOF
  1

  $ ../bin/REPL.exe <<'EOF'
  > 5 > 3
  > EOF
  1

  $ ../bin/REPL.exe <<'EOF'
  > 5 >= 5
  > EOF
  1

  $ ../bin/REPL.exe <<'EOF'
  > if 1 then 10 else 20
  > EOF
  10

  $ ../bin/REPL.exe <<'EOF'
  > if 0 then 10 else 20
  > EOF
  20

  $ ../bin/REPL.exe <<'EOF'
  > if 2 = 2 then 1 else 0
  > EOF
  1

  $ ../bin/REPL.exe <<'EOF'
  > if true then 100 else 200
  > EOF
  100

  $ ../bin/REPL.exe <<'EOF'
  > if false then 100 else 200
  > EOF
  200

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
  > let add x y = x + y in add 3 4
  > EOF
  7

  $ ../bin/REPL.exe <<'EOF'
  > let x = 10 in
  > let f = fun y -> x + y in
  > let x = 100 in
  > f 5
  > EOF
  15

  $ ../bin/REPL.exe <<'EOF'
  > let apply_twice f x = f (f x) in
  > let inc x = x + 1 in
  > apply_twice inc 5
  > EOF
  7

  $ ../bin/REPL.exe <<'EOF'
  > let add x y = x + y in let add5 = add 5 in add5 3 + add5 2
  > EOF
  15

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
  > let rec sum n = if n = 0 then 0 else n + sum (n - 1) in sum 100
  > EOF
  5050

  $ ../bin/REPL.exe <<'EOF'
  > let _ = print 42 in 0
  > EOF
  42
  0

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

  $ ../bin/REPL.exe 50 <<'EOF' 2>&1
  > let rec loop x = loop x in loop 1
  > EOF
  Error: step limit exceeded
  [1]

  $ ../bin/REPL.exe <<'EOF' 2>&1
  > x + 1
  > EOF
  Error: unbound variable x
  [1]

  $ ../bin/REPL.exe <<'EOF' 2>&1
  > 5 3
  > EOF
  Error: not a function
  [1]

  $ ../bin/REPL.exe <<'EOF' 2>&1
  > 10 / 0
  > EOF
  Error: division by zero
  [1]

  $ ../bin/REPL.exe <<'EOF' 2>&1
  > let x = in 5
  > EOF
  Parse error
  [1]

  $ ../bin/REPL.exe <<'EOF' 2>&1
  > if fun x -> x then 1 else 2
  > EOF
  Error: expected an integer
  [1]

  $ ../bin/REPL.exe <<'EOF' 2>&1
  > 1 + (fun x -> x)
  > EOF
  Error: expected an integer
  [1]

  $ ../bin/REPL.exe <<'EOF' 2>&1
  > print (fun x -> x)
  > EOF
  Error: expected an integer
  [1]

  $ ../bin/REPL.exe <<'EOF' 2>&1
  > let rec x = 1 in x
  > EOF
  Error: let rec requires a function on the right-hand side
  [1]
