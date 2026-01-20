Copyright 2021-2024, Kakadu and contributors
SPDX-License-Identifier: CC0-1.0

Cram tests here. They run and compare program output to the expected output
https://dune.readthedocs.io/en/stable/tests.html#cram-tests
Use `dune promote` after you change things that should runned

If you need to put sample program and use it both in your interpreter and preinstalled one,
you could put it into separate file. Thise will need stanza `(cram (deps demo_input.txt))`
in the dune file

  $ ../bin/REPL.exe <<EOF
  > fun f -> x
  TypeError: Tried to return non-integer

  $ ../bin/REPL.exe <<EOF
  > ( fun x -> fun x -> x ) 1 2
  Success: 2

Desugaring an abstraction
  $ ../bin/REPL.exe <<EOF
  > ( fun x y -> x + y ) 1 2
  Success: 3

This code should work after adding arithmetic operators
  $ ../bin/REPL.exe <<EOF
  > (fun x -> ((fun x -> x) 6) + x) 5
  Success: 11

CBV argument evaluation
  $ ../bin/REPL.exe <<EOF
  > (fun x -> x) ((fun  y -> y) 4)
  Success: 4

  $ ../bin/REPL.exe <<EOF
  > (fun x -> (fun y -> x + y)) 5 8
  Success: 13

  $ ../bin/REPL.exe <<EOF
  > (fun x -> x) ((fun y -> y * 2) 9)
  Success: 18

Integer arithmetic

  $ ../bin/REPL.exe <<EOF
  > 42
  Success: 42

  $ ../bin/REPL.exe <<EOF
  > 7 * 6
  Success: 42

  $ ../bin/REPL.exe <<EOF
  > 1 + 2 * 3
  Success: 7

  $ ../bin/REPL.exe <<EOF
  > (4 + 5*10 < (  4 + 5) * 10 - 28)
  Success: 1

Negative numbers
  $ ../bin/REPL.exe <<EOF
  >  1 -2
  Success: -1

  $ ../bin/REPL.exe <<EOF
  >  - (1 -2)
  Success: 1

  $ ../bin/REPL.exe <<EOF
  >  -2
  Success: -2

  $ ../bin/REPL.exe <<EOF
  >  1/0
  Division by zero

Arithmetic and application precedence

  $ ../bin/REPL.exe <<EOF
  >  (fun x -> x) 4 + 5
  Success: 9

  $ ../bin/REPL.exe <<EOF
  >  (fun x -> x + 1) 2 * 3
  Success: 9

Condition
  $ ../bin/REPL.exe <<EOF
  >  if 1 then 2 else 3
  Success: 2

  $ ../bin/REPL.exe <<EOF
  >  if 0 then 2 else 3
  Success: 3

  $ ../bin/REPL.exe <<EOF
  >  if 1 + 1 then 2 * 3 else 3
  Success: 6

  $ ../bin/REPL.exe <<EOF
  >  if 1 then if 0 then 2 else 3 else 4
  Success: 3


  $ ../bin/REPL.exe <<EOF
  >  (if 1 then fun x -> x + 1 else fun x -> x + 2) 4
  Success: 5

  $ ../bin/REPL.exe <<EOF
  >  (if 0 then fun x -> x * 2 else fun x -> x * 3) 5
  Success: 15

  $ ../bin/REPL.exe <<EOF
  >  if (fun x -> x) 1 then 7 else 8
  Success: 7

  $ ../bin/REPL.exe <<EOF
  > (fun x -> if x then x + 1 else x - 1) 4
  Success: 5

let expressions
  $ ../bin/REPL.exe <<EOF
  > let x = 42 in x
  Success: 42

  $ ../bin/REPL.exe <<EOF
  > let double n = n * 2 in double 5
  Success: 10

  $ ../bin/REPL.exe <<EOF
  > let x = 1 in let y = 2 in x + y
  Success: 3

  $ ../bin/REPL.exe <<EOF
  > let f x = if x then 222 else 91 in f 0
  Success: 91

Below we redirect contents of the file to the evaluator
  $ ../bin/REPL.exe   < lam_1+1.txt
  TypeError: Tried to return non-integer

