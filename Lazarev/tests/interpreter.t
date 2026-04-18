Copyright 2021-2024, Kakadu and contributors
SPDX-License-Identifier: CC0-1.0

Cram tests here. They run and compare program output to the expected output
https://dune.readthedocs.io/en/stable/tests.html#cram-tests
Use `dune promote` after you change things that should runned

If you need to put sample program and use it both in your interpreter and preinstalled one,
you could put it into separate file. Thise will need stanza `(cram (deps demo_input.txt))`
in the dune file

Test with no input

  $ ../bin/REPL.exe --help <<EOF
  An interpreter for ML-like language
    --steps Set maximum number of evaluation steps (default: 1000)
    --multiline Enable multiline input for read eval print loop
    -help  Display this list of options
    --help  Display this list of options

  $ ../bin/REPL.exe <<EOF
  No input

  $ ../bin/REPL.exe --steps 100 <<EOF
  No input

Test arithmetic operations

  $ ../bin/REPL.exe <<EOF
  > 12 + 3
  int: 15

  $ ../bin/REPL.exe <<EOF
  > -9 + 14
  int: 5

  $ ../bin/REPL.exe <<EOF
  > -9 * 10 - 10
  int: -100

  $ ../bin/REPL.exe <<EOF
  > 22 + 22 * 22
  int: 506

  $ ../bin/REPL.exe <<EOF
  > (22 + 22) * 2
  int: 88

  $ ../bin/REPL.exe <<EOF
  > 1 / 24
  int: 0

  $ ../bin/REPL.exe <<EOF
  > 47 mod 4
  int: 3

  $ ../bin/REPL.exe <<EOF
  > 11 / (2 + 2 - (8 / 2)) + 1
  Error: Division by zero

  $ ../bin/REPL.exe <<EOF
  > (42) mod (0)
  Error: Division by zero

  $ ../bin/REPL.exe --steps 3 <<EOF
  > 1 + 2 + 3 + 4 + 5
  Error: Steps limit exceeded

  $ ../bin/REPL.exe <<EOF
  > ((1) / (true))
  Error: Types mismatch: 'int' and 'bool'

Test logical & compare operations

  $ ../bin/REPL.exe <<EOF
  > true || false
  bool: true

  $ ../bin/REPL.exe <<EOF
  > !true && false
  bool: false

  $ ../bin/REPL.exe <<EOF
  > !42
  Error: Type mismatch: 'int'

  $ ../bin/REPL.exe --steps 1 <<EOF
  > !(true || false && false && true)
  Error: Steps limit exceeded

  $ ../bin/REPL.exe <<EOF
  > 2 > 1 <> false
  bool: true

  $ ../bin/REPL.exe <<EOF
  > 45 > 100 = (33 <= 33)
  bool: false

  $ ../bin/REPL.exe <<EOF
  > !(true && false) = (!true || !false)
  bool: true

  $ ../bin/REPL.exe <<EOF
  > if (true <> false) then (4 >= 2) else 42
  bool: true

  $ ../bin/REPL.exe <<EOF
  > if (true <> false) then -42 else 42
  int: -42

Test variables

  $ ../bin/REPL.exe <<EOF
  > a + b
  Error: Unbound variable 'a'

Test abstraction & application

  $ ../bin/REPL.exe <<EOF
  > (fun x -> (x + 1) 1)
  int: 2

  $ ../bin/REPL.exe <<EOF
  > (fun x y -> (x + y) 55 45)
  int: 100

  $ ../bin/REPL.exe <<EOF
  > (fun x y -> ((x + 10), (y - 10))) @@ 100 @@ 200
  int * int: 110, 190

  $ ../bin/REPL.exe <<EOF
  > ((fun _ x -> x) 100 200)
  int: 200

  $ ../bin/REPL.exe <<EOF
  > ((fun x -> x) _)
  Error: Unbound variable '_'

  $ ../bin/REPL.exe --multiline <<EOF
  > (print_int 100)
  > (print_bool false)
  100
  unit: ()
  false
  unit: ()

  $ ../bin/REPL.exe --multiline <<EOF
  > print_int
  > print_bool
  > (fst, snd)
  <built-in>: ?
  <built-in>: ?
  <built-in> * <built-in>: ?, ?

  $ ../bin/REPL.exe --multiline <<EOF
  > (print_hoho 100)
  > ((1 + 1) 100)
  Error: Unbound variable 'print_hoho'
  Error: Invalid application

Test tuples of different types

  $ ../bin/REPL.exe <<EOF
  > (1, 2, true, 3)
  int * int * bool * int: 1, 2, true, 3

  $ ../bin/REPL.exe <<EOF
  > (1, (2 + 3), (true || false), (4 + 5))
  int * int * bool * int: 1, 5, true, 9

  $ ../bin/REPL.exe <<EOF
  > (fun _ -> 213, fun x -> (x + 1))
  <closure> * <closure>: ?, ?

  $ ../bin/REPL.exe <<EOF
  > ((11 + 2), (5 * 4), true)
  int * int * bool: 13, 20, true

  $ ../bin/REPL.exe <<EOF
  > ((5 mod 2), false) * (1, 2)
  Error: Types mismatch: 'int * bool' and 'int * int'

Test if-then-else statement

  $ ../bin/REPL.exe <<EOF
  > if 1 = (2 - 1) then 42 else (print_int 1)
  int: 42

  $ ../bin/REPL.exe <<EOF
  > if 1 + 2 <= (2 - 1) then (print_int 1) else 42
  int: 42

  $ ../bin/REPL.exe <<EOF
  > if (1 + 2) then (print_int 1) else 42
  Error: Type mismatch: 'int'

  $ ../bin/REPL.exe <<EOF
  > if () then true else false
  Error: Type mismatch: 'unit'

Test non-recursive let statement & builtin abstraction

  $ ../bin/REPL.exe <<EOF
  > let a = 512 * 2 - 24 in (print_int a)
  1000
  unit: ()

  $ ../bin/REPL.exe --multiline <<EOF
  > let a = let _ = (print_int 10) in 1 in let b = (print_bool true) in (a, b)
  > let a = let _ = (print_bool false) in false in let b = let _ = (print_int 111) in 111 in (a, b)
  10
  true
  int * unit: 1, ()
  false
  111
  bool * int: false, 111

  $ ../bin/REPL.exe <<EOF
  > let make = fun x y -> (x, y) in let t1 = (make 1 2) in let t2 = ((snd t1), (fst t1)) in (t1, t2)
  int * int * int * int: 1, 2, 2, 1

  $ ../bin/REPL.exe <<EOF
  > let id = fun x -> x in ((id false), (id 1))
  bool * int: false, 1

  $ ../bin/REPL.exe <<EOF
  > let a = (let a = (let a = 7389 in a + 11) in a + 600) in a / 20
  int: 400

  $ ../bin/REPL.exe <<EOF
  > let a = 1 in a + b
  Error: Unbound variable 'b'

Test recursive let statement

  $ ../bin/REPL.exe <<EOF
  > let rec sum = fun n -> if (n = 0) then 0 else (n + (sum (n - 1))) in (sum 100)
  int: 5050

  $ ../bin/REPL.exe <<EOF
  > let rec fact = fun n -> (if (n = 0) then 1 else (n * (fact (n - 1)))) in (fact 6)
  int: 720

  $ ../bin/REPL.exe --steps 10 <<EOF
  > let rec fact = fun n -> (if (n = 0) then 1 else (n * (fact (n - 1)))) in (fact 6)
  Error: Steps limit exceeded

  $ ../bin/REPL.exe --steps 500 <<EOF
  > let rec fact = fun n -> (if (n = 0) then 1 else (n * (fact (n - 1)))) in (fact 6)
  int: 720

  $ ../bin/REPL.exe <<EOF
  > let rec fix = fun f s -> (f (fix f) s) in (fix (fun self n -> if (n <= 1) then 1 else (n * (self (n - 1)))) 7)
  int: 5040

  $ ../bin/REPL.exe <<EOF
  > let rec fib = fun n -> if (n <= 2) then 1 else ((fib (n - 1)) + (fib (n - 2))) in (fib 8)
  int: 21

  $ ../bin/REPL.exe <<EOF
  > let rec fib = fun n -> if (n <= 2) then 1 else ((fib (n - 1)) + (fib (n - 2))) in (fib 20)
  int: 6765

Test some infinite recursions

  $ ../bin/REPL.exe --steps 1000 <<EOF
  > let rec hehe = (fun _ -> (1 + (hehe 1))) in (hehe 1)
  Error: Steps limit exceeded

  $ ../bin/REPL.exe --steps 100 <<EOF
  > let omega = ((fun x -> (x x)) (fun x -> (x x))) in omega
  Error: Steps limit exceeded

  $ ../bin/REPL.exe --steps 1000 <<EOF
  > let omega = ((fun x -> (x x)) (fun x -> (x x))) in omega
  Error: Steps limit exceeded

  $ ../bin/REPL.exe --steps 100000 <<EOF
  > let omega = ((fun x -> (x x)) (fun x -> (x x))) in omega
  Error: Steps limit exceeded
