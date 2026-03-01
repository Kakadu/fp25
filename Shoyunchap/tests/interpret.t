Copyright 2021-2025, Kakadu and contributors
SPDX-License-Identifier: CC0-1.0 

Cram tests here. They run and compare program output to the expected output
https://dune.readthedocs.io/en/stable/tests.html#cram-tests
Use `dune promote` after you change things that should runned


  $ ../REPL.exe '2 + 3'
  Result: 5

  $ ../REPL.exe '10 - 4'
  Result: 6

  $ ../REPL.exe '6 * 7'
  Result: 42

  $ ../REPL.exe '15 / 3'
  Result: 5

  $ ../REPL.exe '5 / 0'
  Error: Division by zero

  $ ../REPL.exe '5 = 5'
  Result: 1

  $ ../REPL.exe '3 < 5'
  Result: 1

  $ ../REPL.exe 'if 1 then 10 else 20'
  Result: 10

  $ ../REPL.exe 'if 0 then 10 else 20'
  Result: 20

  $ ../REPL.exe 'fun x -> x * 2'
  Result: <fun>

  $ ../REPL.exe 'let x = 5 in x + 1'
  Result: 6

  $ ../REPL.exe 'let f = fun x -> x * 2 in f 10'
  Result: 20

  $ ../REPL.exe 'let add = fun x y -> x + y in add 3 4'
  Result: 7

  $ ../REPL.exe '2 + 3 * 4'
  Result: 14

  $ ../REPL.exe '(2 + 3) * 4'
  Result: 20

  $ ../REPL.exe 'let rec fact n = if n = 0 then 1 else n * fact (n - 1) in fact 5'
  Result: 120

  $ ../REPL.exe 'let rec fib n = if n <= 1 then n else fib (n - 1) + fib (n - 2) in fib 6'
  Result: 8

  $ ../REPL.exe 'let x = (fun a -> a) in x 3'
  Result: 3

  $ ../REPL.exe 'let x = 3 in x 4'
  Error: Not a function: 3

  $ ../REPL.exe 'if () then 1 else 2'
  Error: Not an int: ()

  $ ../REPL.exe 'if (fun x -> x) then 5 else 0'
  Error: Not an int: <fun>

  $ ../REPL.exe 'if 0 then 5 else 9'
  Result: 9

  $ ../REPL.exe 'if 2 then 5 else 9'
  Result: 5

  $ ../REPL.exe 'fix (fun self -> fun n -> if n = 0 then 1 else n * self (n - 1)) 4'
  Result: 24

  $ ../REPL.exe 'let add x y = x + y in let add5 = add 5 in add5 3 + add5 2'
  Result: 15

  $ ../REPL.exe 'let x = -7 * -8 in let f y = x - y in f 1'
  Result: 55

  $ ../REPL.exe 'let f x = x * x in f 5 5'
  Error: Not a function: 25

  $ ../REPL.exe 'let r = 4 * 8 in if r < 0 then 8'
  Result: ()

  $ ../REPL.exe 'let rec fix f eta = f (fix f) eta in let fact_gen = fun fact -> fun n -> if n = 0 then 1 else n * fact (n - 1) in let fact = fix fact_gen in fact 5'
  Result: 120

  $ ../REPL.exe 'let x = 1 in let x = x + 1 in x'
  Result: 2

  $ ../REPL.exe 'let apply_twice f x = f (f x) in let inc x = x + 1 in apply_twice (inc 5) 7'
  Error: Not a function: 6

  $ ../REPL.exe 'let rec loop x = loop x'
  Result: <fun>

  $ ../REPL.exe 'let rec bad n = bad (n+1) in bad 0'
  Error: Step limit exceeded

  $ ../REPL.exe 'let x c = c * 2 in x 3'
  Result: 6

  $ ../REPL.exe 'let r a = a * a in r (let y = 7)'
  Result: 49

  $ ../REPL.exe 'let rec f n k = if n > k then n + f (n - 1) k else k in f 5 (-1 * 5 + 5)'
  Result: 15

  $ ../REPL.exe 'let rec t n = t (n + 1) in t 4'
  Error: Step limit exceeded

  $ ../REPL.exe 'let r = 5 in let rec f n k = if n > k then n + f (n - 1) k else k in f r 0'
  Result: 15

  $ ../REPL.exe '1 + 2'
  Result: 3

  $ ../REPL.exe '2 - 1'
  Result: 1

  $ ../REPL.exe '1 * 2'
  Result: 2

  $ ../REPL.exe '2 / 2'
  Result: 1

  $ ../REPL.exe 'let x = 1 in x + 1'
  Result: 2

  $ ../REPL.exe '1 < 2'
  Result: 1

  $ ../REPL.exe '2 < 1'
  Result: 0

  $ ../REPL.exe '1 = 1'
  Result: 1

  $ ../REPL.exe '1 > 3'
  Result: 0

  $ ../REPL.exe 'if 1 then 1 else 0'
  Result: 1

  $ ../REPL.exe '2 + 3'
  Result: 5

  $ ../REPL.exe '4 - 2'
  Result: 2

  $ ../REPL.exe '2 * 3'
  Result: 6

  $ ../REPL.exe '6 / 3'
  Result: 2

  $ ../REPL.exe 'let x = 2 in x + 2'
  Result: 4

  $ ../REPL.exe '2 < 3'
  Result: 1

  $ ../REPL.exe '3 < 2'
  Result: 0

  $ ../REPL.exe '2 = 2'
  Result: 1

  $ ../REPL.exe '2 > 4'
  Result: 0

  $ ../REPL.exe 'if 2 then 1 else 0'
  Result: 1

  $ ../REPL.exe '3 + 4'
  Result: 7

  $ ../REPL.exe '6 - 3'
  Result: 3

  $ ../REPL.exe '3 * 4'
  Result: 12

  $ ../REPL.exe '12 / 4'
  Result: 3

  $ ../REPL.exe 'let x = 3 in x + 3'
  Result: 6

  $ ../REPL.exe '3 < 4'
  Result: 1

  $ ../REPL.exe '4 < 3'
  Result: 0

  $ ../REPL.exe '3 = 3'
  Result: 1

  $ ../REPL.exe '3 > 5'
  Result: 0

  $ ../REPL.exe 'if 3 then 1 else 0'
  Result: 1

  $ ../REPL.exe '4 + 5'
  Result: 9

  $ ../REPL.exe '8 - 4'
  Result: 4

  $ ../REPL.exe '4 * 5'
  Result: 20

  $ ../REPL.exe '20 / 5'
  Result: 4

  $ ../REPL.exe 'let x = 4 in x + 4'
  Result: 8

  $ ../REPL.exe '4 < 5'
  Result: 1

  $ ../REPL.exe '5 < 4'
  Result: 0

  $ ../REPL.exe '4 = 4'
  Result: 1

  $ ../REPL.exe '4 > 6'
  Result: 0

  $ ../REPL.exe 'if 4 then 1 else 0'
  Result: 1

  $ ../REPL.exe '5 + 6'
  Result: 11

  $ ../REPL.exe '10 - 5'
  Result: 5

  $ ../REPL.exe '5 * 6'
  Result: 30

  $ ../REPL.exe '30 / 6'
  Result: 5

  $ ../REPL.exe 'let x = 5 in x + 5'
  Result: 10

  $ ../REPL.exe '5 < 6'
  Result: 1

  $ ../REPL.exe '6 < 5'
  Result: 0

  $ ../REPL.exe '5 = 5'
  Result: 1

  $ ../REPL.exe '5 > 7'
  Result: 0

  $ ../REPL.exe 'if 5 then 1 else 0'
  Result: 1

  $ ../REPL.exe '6 + 7'
  Result: 13

  $ ../REPL.exe '12 - 6'
  Result: 6

  $ ../REPL.exe '6 * 7'
  Result: 42

  $ ../REPL.exe '42 / 7'
  Result: 6

  $ ../REPL.exe 'let x = 6 in x + 6'
  Result: 12

  $ ../REPL.exe '6 < 7'
  Result: 1

  $ ../REPL.exe '7 < 6'
  Result: 0

  $ ../REPL.exe '6 = 6'
  Result: 1

  $ ../REPL.exe '6 > 8'
  Result: 0

  $ ../REPL.exe 'if 6 then 1 else 0'
  Result: 1

  $ ../REPL.exe '7 + 8'
  Result: 15

  $ ../REPL.exe '14 - 7'
  Result: 7

  $ ../REPL.exe '7 * 8'
  Result: 56

  $ ../REPL.exe '56 / 8'
  Result: 7

  $ ../REPL.exe 'let x = 7 in x + 7'
  Result: 14

  $ ../REPL.exe '7 < 8'
  Result: 1

  $ ../REPL.exe '8 < 7'
  Result: 0

  $ ../REPL.exe '7 = 7'
  Result: 1

  $ ../REPL.exe '7 > 9'
  Result: 0

  $ ../REPL.exe 'if 7 then 1 else 0'
  Result: 1

  $ ../REPL.exe '8 + 9'
  Result: 17

  $ ../REPL.exe '16 - 8'
  Result: 8

  $ ../REPL.exe '8 * 9'
  Result: 72

  $ ../REPL.exe '72 / 9'
  Result: 8

  $ ../REPL.exe 'let x = 8 in x + 8'
  Result: 16

  $ ../REPL.exe '8 < 9'
  Result: 1

  $ ../REPL.exe '9 < 8'
  Result: 0

  $ ../REPL.exe '8 = 8'
  Result: 1

  $ ../REPL.exe '8 > 10'
  Result: 0

  $ ../REPL.exe 'if 8 then 1 else 0'
  Result: 1

  $ ../REPL.exe '9 + 10'
  Result: 19

  $ ../REPL.exe '18 - 9'
  Result: 9

  $ ../REPL.exe '9 * 10'
  Result: 90

  $ ../REPL.exe '90 / 10'
  Result: 9

  $ ../REPL.exe 'let x = 9 in x + 9'
  Result: 18

  $ ../REPL.exe '9 < 10'
  Result: 1

  $ ../REPL.exe '10 < 9'
  Result: 0

  $ ../REPL.exe '9 = 9'
  Result: 1

  $ ../REPL.exe '9 > 11'
  Result: 0

  $ ../REPL.exe 'if 9 then 1 else 0'
  Result: 1

  $ ../REPL.exe '10 + 11'
  Result: 21

  $ ../REPL.exe '20 - 10'
  Result: 10

  $ ../REPL.exe '10 * 11'
  Result: 110

  $ ../REPL.exe '110 / 11'
  Result: 10

  $ ../REPL.exe 'let x = 10 in x + 10'
  Result: 20

  $ ../REPL.exe '10 < 11'
  Result: 1

  $ ../REPL.exe '11 < 10'
  Result: 0

  $ ../REPL.exe '10 = 10'
  Result: 1

  $ ../REPL.exe '10 > 12'
  Result: 0

  $ ../REPL.exe 'if 10 then 1 else 0'
  Result: 1

  $ ../REPL.exe '11 + 12'
  Result: 23

  $ ../REPL.exe '22 - 11'
  Result: 11

  $ ../REPL.exe '11 * 12'
  Result: 132

  $ ../REPL.exe '132 / 12'
  Result: 11

  $ ../REPL.exe 'let x = 11 in x + 11'
  Result: 22

  $ ../REPL.exe '11 < 12'
  Result: 1

  $ ../REPL.exe '12 < 11'
  Result: 0

  $ ../REPL.exe '11 = 11'
  Result: 1

  $ ../REPL.exe '11 > 13'
  Result: 0

  $ ../REPL.exe 'if 11 then 1 else 0'
  Result: 1

  $ ../REPL.exe '12 + 13'
  Result: 25

  $ ../REPL.exe '24 - 12'
  Result: 12

  $ ../REPL.exe '12 * 13'
  Result: 156

  $ ../REPL.exe '156 / 13'
  Result: 12

  $ ../REPL.exe 'let x = 12 in x + 12'
  Result: 24

  $ ../REPL.exe '12 < 13'
  Result: 1

  $ ../REPL.exe '13 < 12'
  Result: 0

  $ ../REPL.exe '12 = 12'
  Result: 1

  $ ../REPL.exe '12 > 14'
  Result: 0

  $ ../REPL.exe 'if 12 then 1 else 0'
  Result: 1

  $ ../REPL.exe '13 + 14'
  Result: 27

  $ ../REPL.exe '26 - 13'
  Result: 13

  $ ../REPL.exe '13 * 14'
  Result: 182

  $ ../REPL.exe '182 / 14'
  Result: 13

  $ ../REPL.exe 'let x = 13 in x + 13'
  Result: 26

  $ ../REPL.exe '13 < 14'
  Result: 1

  $ ../REPL.exe '14 < 13'
  Result: 0

  $ ../REPL.exe '13 = 13'
  Result: 1

  $ ../REPL.exe '13 > 15'
  Result: 0

  $ ../REPL.exe 'if 13 then 1 else 0'
  Result: 1

  $ ../REPL.exe '14 + 15'
  Result: 29

  $ ../REPL.exe '28 - 14'
  Result: 14

  $ ../REPL.exe '14 * 15'
  Result: 210

  $ ../REPL.exe '210 / 15'
  Result: 14

  $ ../REPL.exe 'let x = 14 in x + 14'
  Result: 28

  $ ../REPL.exe '14 < 15'
  Result: 1

  $ ../REPL.exe '15 < 14'
  Result: 0

  $ ../REPL.exe '14 = 14'
  Result: 1

  $ ../REPL.exe '14 > 16'
  Result: 0

  $ ../REPL.exe 'if 14 then 1 else 0'
  Result: 1

  $ ../REPL.exe '15 + 16'
  Result: 31

  $ ../REPL.exe '30 - 15'
  Result: 15

  $ ../REPL.exe '15 * 16'
  Result: 240

  $ ../REPL.exe '240 / 16'
  Result: 15

  $ ../REPL.exe 'let x = 15 in x + 15'
  Result: 30

  $ ../REPL.exe '15 < 16'
  Result: 1

  $ ../REPL.exe '16 < 15'
  Result: 0

  $ ../REPL.exe '15 = 15'
  Result: 1

  $ ../REPL.exe '15 > 17'
  Result: 0

  $ ../REPL.exe 'if 15 then 1 else 0'
  Result: 1

  $ ../REPL.exe '16 + 17'
  Result: 33

  $ ../REPL.exe '32 - 16'
  Result: 16

  $ ../REPL.exe '16 * 17'
  Result: 272

  $ ../REPL.exe '272 / 17'
  Result: 16

  $ ../REPL.exe 'let x = 16 in x + 16'
  Result: 32

  $ ../REPL.exe '16 < 17'
  Result: 1

  $ ../REPL.exe '17 < 16'
  Result: 0

  $ ../REPL.exe '16 = 16'
  Result: 1

  $ ../REPL.exe '16 > 18'
  Result: 0

  $ ../REPL.exe 'if 16 then 1 else 0'
  Result: 1

  $ ../REPL.exe 'unknown'
  Error: Unbound variable unknown

  $ ../REPL.exe '5 6'
  Error: Not a function: 5

  $ ../REPL.exe '1 / 0'
  Error: Division by zero

  $ ../REPL.exe '1 + ()'
  Error: Not an int: 1

  $ ../REPL.exe 'fix 5'
  Error: fix expects fun self -> fun x -> ...
