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
  
  # let unification failed
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
  
  # true : bool
  # 

Booleans 3
  $ ../bin/REPL.exe << EOF
  > false
  > EOF
  MardukML REPL
  
  # false : bool
  # 

Booleans 3
  $ ../bin/REPL.exe << EOF
  > (if (1 = 2) then false else true)
  > EOF
  MardukML REPL
  
  # true : bool
  # 

Booleans 4
  $ ../bin/REPL.exe << EOF
  > ((true && false) || true)
  > EOF
  MardukML REPL
  
  # true : bool
  # 

Pairs 1
  $ ../bin/REPL.exe << EOF
  > (1, 2)
  > EOF
  MardukML REPL
  
  # (1, 2) : (int * int)
  # 

Pairs 2
  $ ../bin/REPL.exe << EOF
  > (fst (1, true))
  > EOF
  MardukML REPL
  
  # 1 : int
  # 

Pairs 3
  $ ../bin/REPL.exe << EOF
  > (snd (1, true))
  > EOF
  MardukML REPL
  
  # true : bool
  # 

Sum 1
  $ ../bin/REPL.exe << EOF
  > (inl 1)
  > EOF
  MardukML REPL
  
  # (inl 1) : (int + bv1)
  # 

Sum 2
  $ ../bin/REPL.exe << EOF
  > (inr 1)
  > EOF
  MardukML REPL
  
  # (inr 1) : (bv0 + int)
  # 

Sum 3
  $ ../bin/REPL.exe << EOF
  > (match (inl 1) with | inl v -> (println_int v) | inr v -> ())
  > EOF
  MardukML REPL
  
  # 1
  () : unit
  # 

Sum 4
  $ ../bin/REPL.exe << EOF
  > (match (inr 1) with | inl v -> (println_int v) | inr v -> ())
  > EOF
  MardukML REPL
  
  # () : unit
  # 

Test from slides 1
  $ ../bin/REPL.exe << EOF
  > let double f z = (f (f z)) in (((double fun x -> (x+1)) 1), ((double fun x -> (not x)) false))
  > EOF
  MardukML REPL
  
  # (3, false) : (int * bool)
  # 

Test for exceptions: simple case
  $ ../bin/REPL.exe << EOF
  > let exception Exc of unit in (raise (Exc ()))
  > EOF
  MardukML REPL
  
  # Exception: (Exc ()) : bv1
  # 

Test for exceptions: nonexistent exception
  $ ../bin/REPL.exe << EOF
  > let exception Exc of int in (raise Nonexistent)
  > EOF
  MardukML REPL
  
  # unbound variable
  # 

Test for exception something sane 1:
  $ ../bin/REPL.exe << EOF
  > let exception Mulzero of unit in let mul5 x = (if (x = 0) then (raise (Mulzero ())) else (5 * x)) in (mul5 0)
  > EOF
  MardukML REPL
  
  # Exception: (Mulzero ()) : int
  # 

Test for exception something sane 2:
  $ ../bin/REPL.exe << EOF
  > let exception Mulzero of unit in (try let mul5 x = (if (x = 0) then (raise (Mulzero ())) else (5 * x)) in (mul5 0) with | Mulzero v -> (0-1))
  > EOF
  MardukML REPL
  
  # -1 : int
  # 

Test for exception with value:
  $ ../bin/REPL.exe << EOF
  > let exception MyExc of int in (try (raise (MyExc 1)) with | MyExc v -> v)
  > EOF
  MardukML REPL
  
  # 1 : int
  # 

Test for nested exceptions (OMG OCaml allows it):
  $ ../bin/REPL.exe << EOF
  > let exception MyExc1 of bool in let exception MyExc2 of int in (try (raise (MyExc1 (raise (MyExc2 1)))) with | MyExc2 v -> v)
  > EOF
  MardukML REPL
  
  # 1 : int
  # 

Test for exception as a value
  $ ../bin/REPL.exe << EOF
  > let exception MyExc of int in ((fun x -> x) (MyExc 1))
  > EOF
  MardukML REPL
  
  # (MyExc 1) : exc
  # 

Exceptions as return value for factorial
  $ ../bin/REPL.exe << EOF
  > let exception QuickRet of int in \
  > (try \
  > let rec fact n acc = (if (n > 1) then ((fact (n-1)) (acc*n)) else (raise (QuickRet acc))) in ((fact 5) 1) \
  > with | QuickRet val -> val)
  > EOF
  MardukML REPL
  
  # 120 : int
  # 

Division by zero
  $ ../bin/REPL.exe << EOF
  > (1/0)
  > EOF
  MardukML REPL
  
  # Exception: DivisionByZero : int
  # 



TODO: tuples should be printed as tuples
TODO: What is bv14?
  $ ../bin/REPL.exe << EOF
  > let exception Q of int in \
  > let fib n = \
  >   let exception Q of (int * int)  in \
  >   (raise (Q (1, 2))) \
  > in \
  > (fib 8)
  > EOF
  MardukML REPL
  
  # Exception: (Q (\f.((f 1) 2))) : bv14
  # 

