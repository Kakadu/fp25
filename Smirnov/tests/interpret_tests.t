Copyright 2021-2024, Kakadu and contributors
SPDX-License-Identifier: CC0-1.0

Cram tests here. They run and compare program output to the expected output
https://dune.readthedocs.io/en/stable/tests.html#cram-tests
Use `dune promote` after you change things that should runned

If you need to put sample program and use it both in your interpreter and preinstalled one,
you could put it into separate file. Thise will need stanza `(cram (deps demo_input.txt))`
in the dune file

  $ ../bin/REPL.exe << EOF
  > let f = x in f
  > EOF
  MardukML REPL
  
  # unbound variable
  # 

  $ ../bin/REPL.exe << EOF
  > garbage242
  > EOF
  MardukML REPL
  
  # unbound variable
  # 

  $ ../bin/REPL.exe << EOF
  > let f x = x in (f 1)
  > EOF
  MardukML REPL
  
  # 1 : int
  # 

Println test 1
  $ ../bin/REPL.exe << EOF
  > (println_int 5)
  > EOF
  MardukML REPL
  
  # 5
  () : unit
  # 

Println test 2
  $ ../bin/REPL.exe << EOF
  > (println_int x)
  > EOF
  MardukML REPL
  
  # unbound variable
  # 

Explicit fix factorial
  $ ../bin/REPL.exe << EOF
  > let fix f = (fun x -> (f fun v -> ((x x) v)) fun x -> (f fun v -> ((x x) v))) \
  > in let fact = (fix fun f n -> (if (n > 0) then ((f (n-1)) * n) else 1)) \
  > in (fact 5)
  > EOF
  MardukML REPL
  
  # function and args types mismatch
  # 

Simple factorial
  $ ../bin/REPL.exe << EOF
  > let rec fact n = (if (n > 0) then ((fact (n-1)) * n) else 1) \
  > in (fact 5)
  > EOF
  MardukML REPL
  
  # 120 : int
  # 

CPS factorial
  $ ../bin/REPL.exe << EOF
  > let fact n = let rec helper n k = (if (n > 0) then ((helper (n-1)) fun r -> (k (n * r))) else (k 1)) \
  >     in ((helper n) fun n -> n) \
  > in (fact 8)
  > EOF
  MardukML REPL
  
  # 40320 : int
  # 

CPS fib
  $ ../bin/REPL.exe << EOF
  > let fib n = let rec helper n k = (if (n > 1) then ((helper (n-1)) fun r1 -> ((helper (n-2)) fun r2 -> (k (r1+r2)))) else (k n)) \
  >     in ((helper n) fun n -> n) \
  > in (fib 8)
  > EOF
  MardukML REPL
  
  # 21 : int
  # 

Simple fib
  $ ../bin/REPL.exe << EOF
  > let rec fib n = (if (n > 1) then ((fib (n-1)) + (fib (n-2))) else n) \
  > in (fib 8)
  > EOF
  MardukML REPL
  
  # 21 : int
  # 

Booleans 1
  $ ../bin/REPL.exe << EOF
  > true
  > EOF
  MardukML REPL
  
  # (\x.(\y.x)) : bool
  # 

Booleans 3
  $ ../bin/REPL.exe << EOF
  > false
  > EOF
  MardukML REPL
  
  # (\x.(\y.y)) : bool
  # 

Booleans 3
  $ ../bin/REPL.exe << EOF
  > (if (1 == 2) then false else true)
  > EOF
  MardukML REPL
  
  # (\x.(\y.x)) : bool
  # 
