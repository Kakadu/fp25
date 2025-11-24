Copyright 2021-2024, Kakadu and contributors
SPDX-License-Identifier: CC0-1.0

Cram tests here. They run and compare program output to the expected output
https://dune.readthedocs.io/en/stable/tests.html#cram-tests
Use `dune promote` after you change things that should runned

If you need to put sample program and use it both in your interpreter and preinstalled one,
you could put it into separate file. Thise will need stanza `(cram (deps demo_input.txt))`
in the dune file


  $ ../bin/inference_test.exe<<EOF
  > 5 + 5
  (5 + 5)
  TInt

  $ ../bin/inference_test.exe<<EOF
  > let r = 5
  (let r = 5)
  TUnit

  $ ../bin/inference_test.exe<<EOF
  > let r = let t = 5
  (let r = (let t = 5))
  Not an expression

  $ ../bin/inference_test.exe<<EOF
  > let a x = x + x
  (let a = (fun x -> (x + x)))
  TUnit

  $ ../bin/inference_test.exe<<EOF
  > 5 + (fun a -> a+2) 5
  (5 + (fun a -> (a + 2)) 5)
  TInt

  $ ../bin/inference_test.exe<<EOF
  > 5 + (let r = 8) 
  (5 + (let r = 8))
  Not an expression

  $ ../bin/inference_test.exe<<EOF
  > let x n=  n+ 1 in x (let r = 555)
  (let x = (fun n -> (n + 1)) in x (let r = 555))
  Not an expression

  $ ../bin/inference_test.exe<<EOF
  > let r = 5 in let t = 89
  (let r = 5 in (let t = 89))
  TUnit

  $ ../bin/inference_test.exe<<EOF
  > let rec fib n = if n < 2 then 1 else (fib (n-1)) * (fib (n-2)) in fib 10
  (let rec fib = (fun n -> (if (n < 2) then 1 else (fib (n - 1) * fib (n - 2)))) in fib 10)
  TVar(5)

  $ ../bin/inference_test.exe<<EO
  > let rec fib n = if n > 2 then (fib (n-1)) * (fib (n-2)) else 1 in fib 10
  (let rec fib = (fun n -> (if (n > 2) then (fib (n - 1) * fib (n - 2)) else 1)) in fib 10)
  TVar(5)

  $ ../bin/inference_test.exe<<EOF
  > 5 + (let rec r n = if n =0 then n * (r (n+1)) else 1 in r 10)
  (5 + (let rec r = (fun n -> (if (n = 0) then (n * r (n + 1)) else 1)) in r 10))
  TInt

  $ ../bin/inference_test.exe<<EOF
  > let rec f n = f n
  (let rec f = (fun n -> f n))
  Recursive function has infinite type

  $ ../bin/inference_test.exe<<EOF
  > let f n = if n> 0 then n
  (let f = (fun n -> (if (n > 0) then n)))
  TUnit

  $ ../bin/inference_test.exe<<EOF
  > let rec fix f eta = f (fix f) eta in let fact_gen = fun fact -> fun n -> if n = 0 then 1 else n * fact (n - 1) in let fact = fix fact_gen in fact 5
  (let rec fix = (fun f -> (fun eta -> f fix f eta)) in (let fact_gen = (fun fact -> (fun n -> (if (n = 0) then 1 else (n * fact (n - 1))))) in (let fact = fix fact_gen in fact 5)))
  Recursive function has infinite type
