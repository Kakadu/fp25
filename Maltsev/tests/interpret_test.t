Copyright 2021-2024, Kakadu and contributors
SPDX-License-Identifier: CC0-1.0

Cram tests here. They run and compare program output to the expected output
https://dune.readthedocs.io/en/stable/tests.html#cram-tests
Use `dune promote` after you change things that should runned

If you need to put sample program and use it both in your interpreter and preinstalled one,
you could put it into separate file. Thise will need stanza `(cram (deps demo_input.txt))`
in the dune file
  $ ../bin/REPL.exe <<EOF
  > 10 + 10
  
  20


  $ ../bin/REPL.exe <<EOF
  > 1 - 100
  
  -99

  $ ../bin/REPL.exe <<EOF
  > (2 * ((2 +3)))
  
  10


  $ ../bin/REPL.exe <<EOF
  > let a = 10 in a + 20
  
  30
 

  $ ../bin/REPL.exe <<EOF
  > (fun f -> f 10) (fun n -> n)
  
  10

  $ ../bin/REPL.exe <<EOF
  > let rec fact n = if n = 1 then 1 else n * fact (n - 1) in fact 5
  
  120

  $ ../bin/REPL.exe <<EOF
  > let rec fib n = if n = 1 then 0 else if n = 2 then 1 else (fib (n - 1)) + (fib (n - 2)) in fib 7
  
  8

  $ ../bin/REPL.exe <<EOF
  > a
  
  no bound var

  $ ../bin/REPL.exe <<EOF
  > let a b = b + 10 in a
  
  Non integer result

  $ ../bin/REPL.exe <<EOF
  >    let a b c = b / c in a 10 10
  
  1

  $ ../bin/REPL.exe <<EOF
  >    let a b c = b / c in a 10 0
  
  div by zero

  $ ../bin/REPL.exe 1 <<EOF
  >    if 1 then 100 else 99
  
  programm freezed

  $ ../bin/REPL.exe <<EOF
  >    let a = 13 in a 3
  
  can apply functions only

  $ ../bin/REPL.exe <<EOF
  >    let a = 13
  Failed to parse

  $ ../bin/REPL.exe <<EOF
  >    let a b = b + 10 in 1 + a
  
  bad binop

  $ ../bin/REPL.exe <<EOF
  >    let f n = n + 1 in if f then 1 else 0
  
  bad condition

  $ ../bin/REPL.exe <<EOF
  >    let rec a = if 1 != 0 then 1 else 0 in a
  
  not a function

  $ ../bin/REPL.exe 10000 <<EOF
  >    let rec fib n = if n > 2 then fib (n - 1) + fib (n - 2) else if n != 1 then 1 else 0 in fib 7
  
  8

  $ ../bin/REPL.exe <<EOF
  > let f = 0 in f
  
  0

  $ ../bin/REPL.exe <<EOF
  > print 10
  10
  
  Non integer result


  $ ../bin/REPL.exe <<EOF
  > let print = 1 in print
  
  1

  $ ../bin/REPL.exe <<EOF
  > let f = print 10 in f 11
  10
  
  can apply functions only


  $ ../bin/REPL.exe <<EOF
  > let rec fix f x = f (fix f) x in let fixfact fact n = if n = 0 then 1 else n * fact (n - 1) in let fact = fix fixfact in fact 7
  
  5040

  $ ../bin/REPL.exe <<EOF
  > let f n = n in print f
  
  can print ints only

  $ ../bin/REPL.exe <<EOF
  > let f n = if n < 0 then print 0 else print 1 in f -1
  0
  
  Non integer result

  $ ../bin/REPL.exe <<EOF
  > (fun f -> fun x -> f x) (fun n -> n) 10
  
  10
