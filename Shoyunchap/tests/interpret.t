(** Copyright 2021-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

$ dune exec -- ./tests/interpret_cli.exe "1 + 2 * 3"
7

$ dune exec -- ./tests/interpret_cli.exe "5 + 5"
10

$ dune exec -- ./tests/interpret_cli.exe "5 / 0"
Error: Division by zero

$ dune exec -- ./tests/interpret_cli.exe "let r x y = y + x * 8 in r 9 10"
82

$ dune exec -- ./tests/interpret_cli.exe "(fun s k -> s + k) 5 7"
12

$ dune exec -- ./tests/interpret_cli.exe "let r = (fun s k -> s + k) 5 7 in let p = (fun s -> s * 2) ((fun k -> k * 3) 10) in p / 2 + r"
42

$ dune exec -- ./tests/interpret_cli.exe "let x = 7 * 8 + 9 in (fun x -> x + x) 5"
10

$ dune exec -- ./tests/interpret_cli.exe "let x = 7 in let function a b = if x > 4 then x + b else a - b in function 0 1"
8

$ dune exec -- ./tests/interpret_cli.exe "let x = 7 in let y = x in x + y + 8"
22


$ dune exec -- ./tests/interpret_cli.exe "let rec fact n = if n = 0 then 1 else n * fact (n - 1) in fact 5"
120

$ dune exec -- ./tests/interpret_cli.exe "let rec fib n = if n <= 1 then n else fib (n - 1) + fib (n - 2) in fib 10"
55

$ dune exec -- ./tests/interpret_cli.exe "let rec fib n = if n < 2 then 1 else fib (n - 1) + fib (n - 2) in fib 10"
89

$ dune exec -- ./tests/interpret_cli.exe "let x = (fun a -> a) in x 3"
3

$ dune exec -- ./tests/interpret_cli.exe "let x = 3 in x 4"
Error: Not a function: 3

$ dune exec -- ./tests/interpret_cli.exe "let x = 5 in let r y x = x + y * 2 in r x"
<fun>

$ dune exec -- ./tests/interpret_cli.exe "let x = 1 in let x = x + 1 in x"
2

$ dune exec -- ./tests/interpret_cli.exe "let apply_twice f x = f (f x) in let inc x = x + 1 in apply_twice (inc 5) 7"
Error: Not a function: 6

$ dune exec -- ./tests/interpret_cli.exe "if () then 1 else 2"
Error: Not an int: ()

$ dune exec -- ./tests/interpret_cli.exe "if (fun x -> x) then 5 else 0"
Error: Not an int: <fun>

$ dune exec -- ./tests/interpret_cli.exe "if 0 then 5 else 9"
9

$ dune exec -- ./tests/interpret_cli.exe "if 2 then 5 else 9"
5

$ MINIML_MAX_STEPS=5 dune exec -- ./tests/interpret_cli.exe "let rec loop x = loop x in loop 0"
Error: Step limit exceeded

$ dune exec -- ./tests/interpret_cli.exe --steps=8 "let rec bad n = bad (n+1) in bad 0"
Error: Step limit exceeded

$ dune exec -- ./tests/interpret_cli.exe --steps=1 "let x c = c * 2 in x 3"
Error: Step limit exceeded

$ dune exec -- ./tests/interpret_cli.exe "fix (fun self -> fun n -> if n = 0 then 1 else n * self (n - 1)) 4"
24

$ dune exec -- ./tests/interpret_cli.exe "let add x y = x + y in let add5 = add 5 in add5 3 + add5 2"
15

$ dune exec -- ./tests/interpret_cli.exe "let x = -7 * -8 in let f y = x - y in f 1"
55

$ dune exec -- ./tests/interpret_cli.exe "let f x = x * x in f 5 5"
Error: Not a function: 25

$ dune exec -- ./tests/interpret_cli.exe "let r = 4 * 8 in if r < 0 then 8"
()

$ dune exec -- ./tests/interpret_cli.exe "let r a = a * a in r (let y = 7)"
49

$ dune exec -- ./tests/interpret_cli.exe "let t = let r = 8 in r"
8

$ dune exec -- ./tests/interpret_cli.exe "let r = 5 in let rec f n k = if n > k then n + f (n - 1) k else k in let y = if r > 0 then (-1 * r) + 5 else r - 5 in f r y"
15

$ dune exec -- ./tests/interpret_cli.exe --steps=50 "let rec t n = t (n + 1) in t 4"
Error: Step limit exceeded

$ dune exec -- ./tests/interpret_cli.exe "let rec fix f eta = f (fix f) eta in let fact_gen = fun fact -> fun n -> if n = 0 then 1 else n * fact (n - 1) in let fact = fix fact_gen in fact 5"
120
