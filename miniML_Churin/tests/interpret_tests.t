Copyright 2021-2024, Kakadu and contributors
SPDX-License-Identifier: CC0-1.0

Cram tests here. They run and compare program output to the expected output
https://dune.readthedocs.io/en/stable/tests.html#cram-tests
Use `dune promote` after you change things that should runned

If you need to put sample program and use it both in your interpreter and preinstalled one,
you could put it into separate file. Thise will need stanza `(cram (deps demo_input.txt))`
in the dune file

  $ ../bin/REPL.exe <<EOF
  > 1 + 2
  > 3 * 2
  > 4 / 2
  > 9 - 10
  > 10 * 8 - 12
  > 3 + 8 / 4 - 3 * 5
  3
  6
  2
  -1
  68
  -10



  $ ../bin/REPL.exe <<EOF
  > ((1 + 4) * (2 + 2)) / ((5 - 3) * (1 + 1))
  > 1 + 2 * 3 - 4 / 2 + 5 * 6 - 7 / 7
  > 3 * (4 + 5 * (6 - 2)) - 8 / 2 + 7 * (2 + 1)
  > ((15 / 3) + (4 * 2)) * (10 - 6) - (8 + 2) / 5
  > 2 * (3 + 4) - 5 * (6 - 7) + 8 / (9 - 5) * 2
  > (100 - 5 * 4) / (2 + 3) + 7 * 2 - (8 + 9) / 3
  5
  34
  89
  50
  23
  25






  $ ../bin/REPL.exe <<EOF
  > print 15
  > print_int 5
  15
  ()
  5
  ()

  $ ../bin/REPL.exe <<EOF
  > (1 + 2) = 3
  > (3 * 4) <> 12
  > (10 / 2) = 5
  > (8 - 3) = 5
  > (2 + 3) * 4 = 20
  > 2 + 3 * 4 = 14
  1
  0
  1
  1
  1
  1



  $ ../bin/REPL.exe <<EOF
  > ((1 + 2) * 3) - 4 > 5
  > (10 - (2 * 3)) + 5 <> 9
  > ((20 / 4) + 3) * 2 >= 16
  > (15 - (3 + 2)) * 3 >= 30
  > ((8 + 2) / 5) * 10 <= 20
  0
  0
  1
  1
  1


  $ ../bin/REPL.exe <<EOF
  > ((1 + 2) = ((3 * 1) < (4 + 2))) > ((10 / 5) <> (2 * 2))
  > let x = 5 in let y = 3 in (x + y)
  0
  8


  $ ../bin/REPL.exe <<EOF
  > if 1 then 42 else 0
  > if 0 then 42 else 0
  > if 5 > 3 then 100 else 200
  > if 5 < 3 then 100 else 200
  > if 1 + 1 then 42 else 0
  > if 2 * 0 then 42 else 0
  > if 10 - 5 then 100 else 200
  > if 0 / 1 then 42 else 0
  > if (3 + 4) > 6 then 50 else 100
  42
  0
  100
  200
  42
  0
  100
  0
  50

  $ ../bin/REPL.exe <<EOF
  > if (2 + 3) * 4 > 5 * 5 then 100 else 200
  > if (10 - 3) * 2 = 7 + 7 then 50 else 100
  > if (15 / 3) + 2 < 8 - 1 then 30 else 60
  > if (6 * 2) / 3 >= 4 then 25 else 50
  200
  50
  60
  25


  $ ../bin/REPL.exe <<EOF
  > let x = 5 in if x > 3 then x * 2 else x
  > let x = 2 in let y = 3 in if x + y > 4 then x * y else x + y
  > let x = 0 in if x then 42 else (let y = 10 in y)
  > let f = fun x -> x * 2 in if f 5 > 8 then f 10 else f 3
  > let a = 5 in let b = 3 in let c = 2 in if (a + b) > c * 4 then if a - b > 0 then (a * b) - c else (a + b) * c else if c * c = a then a + b + c else a * c
  > let x = 10 in let y = 5 in let z = 2 in if (x / y) * z > x - y then if x mod y = 0 then (x / y) * (y + z) else x + y + z else if z * z = y then x * z else y * z
  10
  6
  10
  20
  10
  10


  $ ../bin/REPL.exe <<EOF
  > (fun x -> x) 42
  > (fun x -> x + 1) 5
  > (fun x -> fun y -> x + y) 3 4
  > (fun x y -> x + y) 3 4
  42
  6
  7
  7


  $ ../bin/REPL.exe <<EOF
  > (fun f -> fun x -> f (f x)) (fun n -> n + 2) 5
  > (fun f -> fun x -> f (f (f x))) (fun n -> n * 2) 3
  > (fun f -> fun g -> fun x -> f (g x)) (fun n -> n + 1) (fun n -> n * 2) 5
  > (fun f -> fun g -> fun x -> g (f x)) (fun n -> n + 1) (fun n -> n * 2) 5
  9
  24
  11
  12


  $ ../bin/REPL.exe <<EOF
  > let compose = fun f -> fun g -> fun x -> f (g x) in let double = fun x -> x * 2 in let inc = fun x -> x + 1 in compose inc double 5
  > let apply = fun f -> fun x -> f x in let double = fun x -> x * 2 in apply double 10
  11
  20
  $ ../bin/REPL.exe <<EOF
  > let rec fact = fun n -> if n <= 1 then 1 else n * fact (n - 1) in fact 5
  > let rec fact = fun n -> if n <= 1 then 1 else n * fact (n - 1) in fact 10
  > let rec fib = fun n -> if n <= 1 then n else fib (n - 1) + fib (n - 2) in fib 10
  > let rec pow = fun x -> fun n -> if n <= 0 then 1 else x * pow x (n - 1) in pow 2 10
  120
  3628800
  55
  1024


  $ ../bin/REPL.exe <<EOF
  > fix (fun f -> fun n -> if n <= 1 then 1 else n * f (n - 1)) 5
  > fix (fun f -> fun n -> if n <= 1 then n else f (n - 1) + f (n - 2)) 10
  > fix (fun f -> fun x -> f x) 42
  120
  55
  Error: Step limit exceeded



  $ ../bin/REPL.exe <<EOF
  > (fun x -> x (fun y -> y)) (fun f -> f 42)
  > (fun f -> f (fun x -> x)) (fun g -> g 42)
  42
  42




  $ ../bin/REPL.exe <<EOF
  > 1 + (fun x -> x)
  > - (fun x -> x)
  > let x = 5 in x 3
  > let rec x = 5 in x 
  > if () then 1 else 0
  > if (fun x -> x) then 1 else 0
  Error: Expected integer, got <fun x>
  Error: Expected integer, got <fun x>
  Error: Expected function, got 5
  Error: Expected function, got ()
  Error: Invalid condition (expected integer): ()
  Error: Invalid condition (expected integer): <fun x>


  $ ../bin/REPL.exe <<EOF
  > let x = 5 in x + y
  > let f x = x + y in f 3
  Error: Unbound variable: y
  Error: Unbound variable: y



  $ ../bin/REPL.exe <<EOF
  > let x = 0 in 5 / x
  > (fun x -> 10 / x) 0
  Error: Division by zero
  Error: Division by zero


  $ ../bin/REPL.exe <<EOF
  > let rec inf = fun x -> inf x in inf 0
  Error: Step limit exceeded



  $ ../bin/REPL.exe <<EOF
  > let rec loop = fun x -> loop x in loop 42
  > let rec forever = fun f -> forever f in forever (fun x -> x)
  > fix (fun f -> f)
  Error: Step limit exceeded
  Error: Step limit exceeded
  Error: fix applied to non-function
