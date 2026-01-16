Copyright 2021-2024, Kakadu and contributors
SPDX-License-Identifier: CC0-1.0

Cram tests here. They run and compare program output to the expected output
https://dune.readthedocs.io/en/stable/tests.html#cram-tests
Use `dune promote` after you change things that should runned


$ dune exec -- ./REPL.exe <<'EOF'
> 2 + 3
EOF
Result: 5

$ dune exec -- ./REPL.exe <<'EOF'
> 10 - 4
EOF
Result: 6

$ dune exec -- ./REPL.exe <<'EOF'
> 6 * 7
EOF
Result: 42

$ dune exec -- ./REPL.exe <<'EOF'
> 15 / 3
EOF
Result: 5

$ dune exec -- ./REPL.exe <<'EOF'
> 5 / 0
EOF
Error: Division by zero

$ dune exec -- ./REPL.exe <<'EOF'
> 5 = 5
EOF
Result: 1

$ dune exec -- ./REPL.exe <<'EOF'
> 3 < 5
EOF
Result: 1

$ dune exec -- ./REPL.exe <<'EOF'
> if 1 then 10 else 20
EOF
Result: 10

$ dune exec -- ./REPL.exe <<'EOF'
> if 0 then 10 else 20
EOF
Result: 20

$ dune exec -- ./REPL.exe <<'EOF'
> fun x -> x * 2
EOF
Result: <fun>

$ dune exec -- ./REPL.exe <<'EOF'
> let x = 5 in x + 1
EOF
Result: 6

$ dune exec -- ./REPL.exe <<'EOF'
> let f = fun x -> x * 2 in f 10
EOF
Result: 20

$ dune exec -- ./REPL.exe <<'EOF'
> let add = fun x y -> x + y in add 3 4
EOF
Result: 7

$ dune exec -- ./REPL.exe <<'EOF'
> 2 + 3 * 4
EOF
Result: 14

$ dune exec -- ./REPL.exe <<'EOF'
> (2 + 3) * 4
EOF
Result: 20

$ dune exec -- ./REPL.exe <<'EOF'
> let rec fact n = if n = 0 then 1 else n * fact (n - 1) in fact 5
EOF
Result: 120

$ dune exec -- ./REPL.exe <<'EOF'
> let rec fib n = if n <= 1 then n else fib (n - 1) + fib (n - 2) in fib 6
EOF
Result: 8

$ dune exec -- ./REPL.exe <<'EOF'
> let x = (fun a -> a) in x 3
EOF
Result: 3

$ dune exec -- ./REPL.exe <<'EOF'
> let x = 3 in x 4
EOF
Error: Not a function: 3

$ dune exec -- ./REPL.exe <<'EOF'
> if () then 1 else 2
EOF
Error: Not an int: ()

$ dune exec -- ./REPL.exe <<'EOF'
> if (fun x -> x) then 5 else 0
EOF
Error: Not an int: <fun>

$ dune exec -- ./REPL.exe <<'EOF'
> if 0 then 5 else 9
EOF
Result: 9

$ dune exec -- ./REPL.exe <<'EOF'
> if 2 then 5 else 9
EOF
Result: 5

$ dune exec -- ./REPL.exe <<'EOF'
> fix (fun self -> fun n -> if n = 0 then 1 else n * self (n - 1)) 4
EOF
Result: 24

$ dune exec -- ./REPL.exe <<'EOF'
> let add x y = x + y in let add5 = add 5 in add5 3 + add5 2
EOF
Result: 15

$ dune exec -- ./REPL.exe <<'EOF'
> let x = -7 * -8 in let f y = x - y in f 1
EOF
Result: 55

$ dune exec -- ./REPL.exe <<'EOF'
> let f x = x * x in f 5 5
EOF
Error: Not a function: 25

$ dune exec -- ./REPL.exe <<'EOF'
> let r = 4 * 8 in if r < 0 then 8
EOF
Result: ()

$ dune exec -- ./REPL.exe <<'EOF'
> let rec fix f eta = f (fix f) eta in let fact_gen = fun fact -> fun n -> if n = 0 then 1 else n * fact (n - 1) in let fact = fix fact_gen in fact 5
EOF
Result: 120

$ dune exec -- ./REPL.exe <<'EOF'
> let x = 1 in let x = x + 1 in x
EOF
Result: 2

$ dune exec -- ./REPL.exe <<'EOF'
> let apply_twice f x = f (f x) in let inc x = x + 1 in apply_twice (inc 5) 7
EOF
Error: Not a function: 6

$ dune exec -- ./REPL.exe <<'EOF'
> let rec loop x = loop x
EOF
Error: Step limit exceeded

$ dune exec -- ./REPL.exe <<'EOF'
> let rec bad n = bad (n+1) in bad 0
EOF
Error: Step limit exceeded

$ dune exec -- ./REPL.exe <<'EOF'
> let x c = c * 2 in x 3
EOF
Result: 6

$ dune exec -- ./REPL.exe <<'EOF'
> let r a = a * a in r (let y = 7)
EOF
Result: 49

$ dune exec -- ./REPL.exe <<'EOF'
> let rec f n k = if n > k then n + f (n - 1) k else k in f 5 (-1 * 5 + 5)
EOF
Result: 15

$ dune exec -- ./REPL.exe <<'EOF'
> let rec t n = t (n + 1) in t 4
EOF
Error: Step limit exceeded

$ dune exec -- ./REPL.exe <<'EOF'
> let r = 5 in let rec f n k = if n > k then n + f (n - 1) k else k in f r 0
EOF
Result: 15

$ dune exec -- ./REPL.exe <<'EOF'
> 1 + 2
EOF
Result: 3

$ dune exec -- ./REPL.exe <<'EOF'
> 2 - 1
EOF
Result: 1

$ dune exec -- ./REPL.exe <<'EOF'
> 1 * 2
EOF
Result: 2

$ dune exec -- ./REPL.exe <<'EOF'
> 2 / 2
EOF
Result: 1

$ dune exec -- ./REPL.exe <<'EOF'
> let x = 1 in x + 1
EOF
Result: 2

$ dune exec -- ./REPL.exe <<'EOF'
> 1 < 2
EOF
Result: 1

$ dune exec -- ./REPL.exe <<'EOF'
> 2 < 1
EOF
Result: 0

$ dune exec -- ./REPL.exe <<'EOF'
> 1 = 1
EOF
Result: 1

$ dune exec -- ./REPL.exe <<'EOF'
> 1 > 3
EOF
Result: 0

$ dune exec -- ./REPL.exe <<'EOF'
> if 1 then 1 else 0
EOF
Result: 1

$ dune exec -- ./REPL.exe <<'EOF'
> 2 + 3
EOF
Result: 5

$ dune exec -- ./REPL.exe <<'EOF'
> 4 - 2
EOF
Result: 2

$ dune exec -- ./REPL.exe <<'EOF'
> 2 * 3
EOF
Result: 6

$ dune exec -- ./REPL.exe <<'EOF'
> 6 / 3
EOF
Result: 2

$ dune exec -- ./REPL.exe <<'EOF'
> let x = 2 in x + 2
EOF
Result: 4

$ dune exec -- ./REPL.exe <<'EOF'
> 2 < 3
EOF
Result: 1

$ dune exec -- ./REPL.exe <<'EOF'
> 3 < 2
EOF
Result: 0

$ dune exec -- ./REPL.exe <<'EOF'
> 2 = 2
EOF
Result: 1

$ dune exec -- ./REPL.exe <<'EOF'
> 2 > 4
EOF
Result: 0

$ dune exec -- ./REPL.exe <<'EOF'
> if 2 then 1 else 0
EOF
Result: 1

$ dune exec -- ./REPL.exe <<'EOF'
> 3 + 4
EOF
Result: 7

$ dune exec -- ./REPL.exe <<'EOF'
> 6 - 3
EOF
Result: 3

$ dune exec -- ./REPL.exe <<'EOF'
> 3 * 4
EOF
Result: 12

$ dune exec -- ./REPL.exe <<'EOF'
> 12 / 4
EOF
Result: 3

$ dune exec -- ./REPL.exe <<'EOF'
> let x = 3 in x + 3
EOF
Result: 6

$ dune exec -- ./REPL.exe <<'EOF'
> 3 < 4
EOF
Result: 1

$ dune exec -- ./REPL.exe <<'EOF'
> 4 < 3
EOF
Result: 0

$ dune exec -- ./REPL.exe <<'EOF'
> 3 = 3
EOF
Result: 1

$ dune exec -- ./REPL.exe <<'EOF'
> 3 > 5
EOF
Result: 0

$ dune exec -- ./REPL.exe <<'EOF'
> if 3 then 1 else 0
EOF
Result: 1

$ dune exec -- ./REPL.exe <<'EOF'
> 4 + 5
EOF
Result: 9

$ dune exec -- ./REPL.exe <<'EOF'
> 8 - 4
EOF
Result: 4

$ dune exec -- ./REPL.exe <<'EOF'
> 4 * 5
EOF
Result: 20

$ dune exec -- ./REPL.exe <<'EOF'
> 20 / 5
EOF
Result: 4

$ dune exec -- ./REPL.exe <<'EOF'
> let x = 4 in x + 4
EOF
Result: 8

$ dune exec -- ./REPL.exe <<'EOF'
> 4 < 5
EOF
Result: 1

$ dune exec -- ./REPL.exe <<'EOF'
> 5 < 4
EOF
Result: 0

$ dune exec -- ./REPL.exe <<'EOF'
> 4 = 4
EOF
Result: 1

$ dune exec -- ./REPL.exe <<'EOF'
> 4 > 6
EOF
Result: 0

$ dune exec -- ./REPL.exe <<'EOF'
> if 4 then 1 else 0
EOF
Result: 1

$ dune exec -- ./REPL.exe <<'EOF'
> 5 + 6
EOF
Result: 11

$ dune exec -- ./REPL.exe <<'EOF'
> 10 - 5
EOF
Result: 5

$ dune exec -- ./REPL.exe <<'EOF'
> 5 * 6
EOF
Result: 30

$ dune exec -- ./REPL.exe <<'EOF'
> 30 / 6
EOF
Result: 5

$ dune exec -- ./REPL.exe <<'EOF'
> let x = 5 in x + 5
EOF
Result: 10

$ dune exec -- ./REPL.exe <<'EOF'
> 5 < 6
EOF
Result: 1

$ dune exec -- ./REPL.exe <<'EOF'
> 6 < 5
EOF
Result: 0

$ dune exec -- ./REPL.exe <<'EOF'
> 5 = 5
EOF
Result: 1

$ dune exec -- ./REPL.exe <<'EOF'
> 5 > 7
EOF
Result: 0

$ dune exec -- ./REPL.exe <<'EOF'
> if 5 then 1 else 0
EOF
Result: 1

$ dune exec -- ./REPL.exe <<'EOF'
> 6 + 7
EOF
Result: 13

$ dune exec -- ./REPL.exe <<'EOF'
> 12 - 6
EOF
Result: 6

$ dune exec -- ./REPL.exe <<'EOF'
> 6 * 7
EOF
Result: 42

$ dune exec -- ./REPL.exe <<'EOF'
> 42 / 7
EOF
Result: 6

$ dune exec -- ./REPL.exe <<'EOF'
> let x = 6 in x + 6
EOF
Result: 12

$ dune exec -- ./REPL.exe <<'EOF'
> 6 < 7
EOF
Result: 1

$ dune exec -- ./REPL.exe <<'EOF'
> 7 < 6
EOF
Result: 0

$ dune exec -- ./REPL.exe <<'EOF'
> 6 = 6
EOF
Result: 1

$ dune exec -- ./REPL.exe <<'EOF'
> 6 > 8
EOF
Result: 0

$ dune exec -- ./REPL.exe <<'EOF'
> if 6 then 1 else 0
EOF
Result: 1

$ dune exec -- ./REPL.exe <<'EOF'
> 7 + 8
EOF
Result: 15

$ dune exec -- ./REPL.exe <<'EOF'
> 14 - 7
EOF
Result: 7

$ dune exec -- ./REPL.exe <<'EOF'
> 7 * 8
EOF
Result: 56

$ dune exec -- ./REPL.exe <<'EOF'
> 56 / 8
EOF
Result: 7

$ dune exec -- ./REPL.exe <<'EOF'
> let x = 7 in x + 7
EOF
Result: 14

$ dune exec -- ./REPL.exe <<'EOF'
> 7 < 8
EOF
Result: 1

$ dune exec -- ./REPL.exe <<'EOF'
> 8 < 7
EOF
Result: 0

$ dune exec -- ./REPL.exe <<'EOF'
> 7 = 7
EOF
Result: 1

$ dune exec -- ./REPL.exe <<'EOF'
> 7 > 9
EOF
Result: 0

$ dune exec -- ./REPL.exe <<'EOF'
> if 7 then 1 else 0
EOF
Result: 1

$ dune exec -- ./REPL.exe <<'EOF'
> 8 + 9
EOF
Result: 17

$ dune exec -- ./REPL.exe <<'EOF'
> 16 - 8
EOF
Result: 8

$ dune exec -- ./REPL.exe <<'EOF'
> 8 * 9
EOF
Result: 72

$ dune exec -- ./REPL.exe <<'EOF'
> 72 / 9
EOF
Result: 8

$ dune exec -- ./REPL.exe <<'EOF'
> let x = 8 in x + 8
EOF
Result: 16

$ dune exec -- ./REPL.exe <<'EOF'
> 8 < 9
EOF
Result: 1

$ dune exec -- ./REPL.exe <<'EOF'
> 9 < 8
EOF
Result: 0

$ dune exec -- ./REPL.exe <<'EOF'
> 8 = 8
EOF
Result: 1

$ dune exec -- ./REPL.exe <<'EOF'
> 8 > 10
EOF
Result: 0

$ dune exec -- ./REPL.exe <<'EOF'
> if 8 then 1 else 0
EOF
Result: 1

$ dune exec -- ./REPL.exe <<'EOF'
> 9 + 10
EOF
Result: 19

$ dune exec -- ./REPL.exe <<'EOF'
> 18 - 9
EOF
Result: 9

$ dune exec -- ./REPL.exe <<'EOF'
> 9 * 10
EOF
Result: 90

$ dune exec -- ./REPL.exe <<'EOF'
> 90 / 10
EOF
Result: 9

$ dune exec -- ./REPL.exe <<'EOF'
> let x = 9 in x + 9
EOF
Result: 18

$ dune exec -- ./REPL.exe <<'EOF'
> 9 < 10
EOF
Result: 1

$ dune exec -- ./REPL.exe <<'EOF'
> 10 < 9
EOF
Result: 0

$ dune exec -- ./REPL.exe <<'EOF'
> 9 = 9
EOF
Result: 1

$ dune exec -- ./REPL.exe <<'EOF'
> 9 > 11
EOF
Result: 0

$ dune exec -- ./REPL.exe <<'EOF'
> if 9 then 1 else 0
EOF
Result: 1

$ dune exec -- ./REPL.exe <<'EOF'
> 10 + 11
EOF
Result: 21

$ dune exec -- ./REPL.exe <<'EOF'
> 20 - 10
EOF
Result: 10

$ dune exec -- ./REPL.exe <<'EOF'
> 10 * 11
EOF
Result: 110

$ dune exec -- ./REPL.exe <<'EOF'
> 110 / 11
EOF
Result: 10

$ dune exec -- ./REPL.exe <<'EOF'
> let x = 10 in x + 10
EOF
Result: 20

$ dune exec -- ./REPL.exe <<'EOF'
> 10 < 11
EOF
Result: 1

$ dune exec -- ./REPL.exe <<'EOF'
> 11 < 10
EOF
Result: 0

$ dune exec -- ./REPL.exe <<'EOF'
> 10 = 10
EOF
Result: 1

$ dune exec -- ./REPL.exe <<'EOF'
> 10 > 12
EOF
Result: 0

$ dune exec -- ./REPL.exe <<'EOF'
> if 10 then 1 else 0
EOF
Result: 1

$ dune exec -- ./REPL.exe <<'EOF'
> 11 + 12
EOF
Result: 23

$ dune exec -- ./REPL.exe <<'EOF'
> 22 - 11
EOF
Result: 11

$ dune exec -- ./REPL.exe <<'EOF'
> 11 * 12
EOF
Result: 132

$ dune exec -- ./REPL.exe <<'EOF'
> 132 / 12
EOF
Result: 11

$ dune exec -- ./REPL.exe <<'EOF'
> let x = 11 in x + 11
EOF
Result: 22

$ dune exec -- ./REPL.exe <<'EOF'
> 11 < 12
EOF
Result: 1

$ dune exec -- ./REPL.exe <<'EOF'
> 12 < 11
EOF
Result: 0

$ dune exec -- ./REPL.exe <<'EOF'
> 11 = 11
EOF
Result: 1

$ dune exec -- ./REPL.exe <<'EOF'
> 11 > 13
EOF
Result: 0

$ dune exec -- ./REPL.exe <<'EOF'
> if 11 then 1 else 0
EOF
Result: 1

$ dune exec -- ./REPL.exe <<'EOF'
> 12 + 13
EOF
Result: 25

$ dune exec -- ./REPL.exe <<'EOF'
> 24 - 12
EOF
Result: 12

$ dune exec -- ./REPL.exe <<'EOF'
> 12 * 13
EOF
Result: 156

$ dune exec -- ./REPL.exe <<'EOF'
> 156 / 13
EOF
Result: 12

$ dune exec -- ./REPL.exe <<'EOF'
> let x = 12 in x + 12
EOF
Result: 24

$ dune exec -- ./REPL.exe <<'EOF'
> 12 < 13
EOF
Result: 1

$ dune exec -- ./REPL.exe <<'EOF'
> 13 < 12
EOF
Result: 0

$ dune exec -- ./REPL.exe <<'EOF'
> 12 = 12
EOF
Result: 1

$ dune exec -- ./REPL.exe <<'EOF'
> 12 > 14
EOF
Result: 0

$ dune exec -- ./REPL.exe <<'EOF'
> if 12 then 1 else 0
EOF
Result: 1

$ dune exec -- ./REPL.exe <<'EOF'
> 13 + 14
EOF
Result: 27

$ dune exec -- ./REPL.exe <<'EOF'
> 26 - 13
EOF
Result: 13

$ dune exec -- ./REPL.exe <<'EOF'
> 13 * 14
EOF
Result: 182

$ dune exec -- ./REPL.exe <<'EOF'
> 182 / 14
EOF
Result: 13

$ dune exec -- ./REPL.exe <<'EOF'
> let x = 13 in x + 13
EOF
Result: 26

$ dune exec -- ./REPL.exe <<'EOF'
> 13 < 14
EOF
Result: 1

$ dune exec -- ./REPL.exe <<'EOF'
> 14 < 13
EOF
Result: 0

$ dune exec -- ./REPL.exe <<'EOF'
> 13 = 13
EOF
Result: 1

$ dune exec -- ./REPL.exe <<'EOF'
> 13 > 15
EOF
Result: 0

$ dune exec -- ./REPL.exe <<'EOF'
> if 13 then 1 else 0
EOF
Result: 1

$ dune exec -- ./REPL.exe <<'EOF'
> 14 + 15
EOF
Result: 29

$ dune exec -- ./REPL.exe <<'EOF'
> 28 - 14
EOF
Result: 14

$ dune exec -- ./REPL.exe <<'EOF'
> 14 * 15
EOF
Result: 210

$ dune exec -- ./REPL.exe <<'EOF'
> 210 / 15
EOF
Result: 14

$ dune exec -- ./REPL.exe <<'EOF'
> let x = 14 in x + 14
EOF
Result: 28

$ dune exec -- ./REPL.exe <<'EOF'
> 14 < 15
EOF
Result: 1

$ dune exec -- ./REPL.exe <<'EOF'
> 15 < 14
EOF
Result: 0

$ dune exec -- ./REPL.exe <<'EOF'
> 14 = 14
EOF
Result: 1

$ dune exec -- ./REPL.exe <<'EOF'
> 14 > 16
EOF
Result: 0

$ dune exec -- ./REPL.exe <<'EOF'
> if 14 then 1 else 0
EOF
Result: 1

$ dune exec -- ./REPL.exe <<'EOF'
> 15 + 16
EOF
Result: 31

$ dune exec -- ./REPL.exe <<'EOF'
> 30 - 15
EOF
Result: 15

$ dune exec -- ./REPL.exe <<'EOF'
> 15 * 16
EOF
Result: 240

$ dune exec -- ./REPL.exe <<'EOF'
> 240 / 16
EOF
Result: 15

$ dune exec -- ./REPL.exe <<'EOF'
> let x = 15 in x + 15
EOF
Result: 30

$ dune exec -- ./REPL.exe <<'EOF'
> 15 < 16
EOF
Result: 1

$ dune exec -- ./REPL.exe <<'EOF'
> 16 < 15
EOF
Result: 0

$ dune exec -- ./REPL.exe <<'EOF'
> 15 = 15
EOF
Result: 1

$ dune exec -- ./REPL.exe <<'EOF'
> 15 > 17
EOF
Result: 0

$ dune exec -- ./REPL.exe <<'EOF'
> if 15 then 1 else 0
EOF
Result: 1

$ dune exec -- ./REPL.exe <<'EOF'
> 16 + 17
EOF
Result: 33

$ dune exec -- ./REPL.exe <<'EOF'
> 32 - 16
EOF
Result: 16

$ dune exec -- ./REPL.exe <<'EOF'
> 16 * 17
EOF
Result: 272

$ dune exec -- ./REPL.exe <<'EOF'
> 272 / 17
EOF
Result: 16

$ dune exec -- ./REPL.exe <<'EOF'
> let x = 16 in x + 16
EOF
Result: 32

$ dune exec -- ./REPL.exe <<'EOF'
> 16 < 17
EOF
Result: 1

$ dune exec -- ./REPL.exe <<'EOF'
> 17 < 16
EOF
Result: 0

$ dune exec -- ./REPL.exe <<'EOF'
> 16 = 16
EOF
Result: 1

$ dune exec -- ./REPL.exe <<'EOF'
> 16 > 18
EOF
Result: 0

$ dune exec -- ./REPL.exe <<'EOF'
> if 16 then 1 else 0
EOF
Result: 1

$ dune exec -- ./REPL.exe <<'EOF'
> unknown
EOF
Error: Unbound variable unknown

$ dune exec -- ./REPL.exe <<'EOF'
> 5 6
EOF
Error: Not a function: 5

$ dune exec -- ./REPL.exe <<'EOF'
> 1 / 0
EOF
Error: Division by zero

$ dune exec -- ./REPL.exe <<'EOF'
> 1 + ()
EOF
Error: Not an int: 1

$ dune exec -- ./REPL.exe <<'EOF'
> fix 5
EOF
Error: fix expects fun self -> fun x -> ...
