Copyright 2021-2024, Kakadu and contributors
SPDX-License-Identifier: CC0-1.0

Cram tests here. They run and compare program output to the expected output
https://dune.readthedocs.io/en/stable/tests.html#cram-tests
Use `dune promote` after you change things that should runned

If you need to put sample program and use it both in your interpreter and preinstalled one,
you could put it into separate file. Thise will need stanza `(cram (deps demo_input.txt))`
in the dune file

  $ ../bin/REPL.exe <<EOF
  > 5 + 5
  (5 + 5)
  10
  $ ../bin/REPL.exe <<EOF
  > let r x y= y+x*8 in r 9 10
  (let r = (fun x -> (fun y -> (y + (x * 8)))) in r 9 10)
  82

  $ ../bin/REPL.exe <<EOF
  > (fun s k -> s+k) 5 7
  (fun s -> (fun k -> (s + k))) 5 7
  12

  $ ../bin/REPL.exe <<EOF
  > let r = (fun s k -> s+k) 5 7 in let p = (fun s->s*2) ((fun k -> k*3) 10) in p/2 + r
  (let r = (fun s -> (fun k -> (s + k))) 5 7 in (let p = (fun s -> (s * 2)) (fun k -> (k * 3)) 10 in ((p / 2) + r)))
  42

  $ ../bin/REPL.exe <<EOF
  > let x = 7*8+9 in (fun x -> x+x) 5
  (let x = ((7 * 8) + 9) in (fun x -> (x + x)) 5)
  10

  $ ../bin/REPL.exe <<EOF
  > let x = 7 in let function a b = if x > 4 then x+b else a-b in function 0 1
  (let x = 7 in (let function = (fun a -> (fun b -> (if (x > 4) then (x + b) else (a - b)))) in function 0 1))
  8

  $ ../bin/REPL.exe <<EOF
  > let x = 7 in let y = x in x+y+8
  (let x = 7 in (let y = x in ((x + y) + 8)))
  22

  $ ../bin/REPL.exe <<EOF
  > let rec fac n = if n < 1 then 1 else (fac (n-1)) * n in fac 5
  (let rec fac = (fun n -> (if (n < 1) then 1 else (fac (n - 1) * n))) in fac 5)
  120

  $ ../bin/REPL.exe <<EOF
  > let rec fib n = if n < 2 then 1 else (fib (n-1)) + (fib (n-2)) in fib 10
  (let rec fib = (fun n -> (if (n < 2) then 1 else (fib (n - 1) + fib (n - 2)))) in fib 10)
  89

  $ ../bin/REPL.exe <<EOF
  > let x = 5 in let r y x= x+y*2 in r x
  (let x = 5 in (let r = (fun y -> (fun x -> (x + (y * 2)))) in r x))
  (fun x -> (x + (5 * 2)))

  $ ../bin/REPL.exe <<EOF
  > let add x y = x + y in let add5 = add 5 in add5 3 + add5 2
  (let add = (fun x -> (fun y -> (x + y))) in (let add5 = add 5 in (add5 3 + add5 2)))
  15

  $ ../bin/REPL.exe <<EOF
  > let apply_twice f x = f (f x) in let inc x = x + 1 in apply_twice (inc 5) 7
  (let apply_twice = (fun f -> (fun x -> f f x)) in (let inc = (fun x -> (x + 1)) in apply_twice inc 5 7))
  can only apply args to funcs

  $ ../bin/REPL.exe <<EOF
  > let x = -7 * -8 in let f y = x - y in f 1
  (let x = (-7 * -8) in (let f = (fun y -> (x - y)) in f 1))
  55

  $ ../bin/REPL.exe <<EOF
  > let f x = x * x in f 5 5
  (let f = (fun x -> (x * x)) in f 5 5)
  Too many args for function

  $ ../bin/REPL.exe <<EOF
  > if (let s = 5) then 5
  (if (let s = 5) then 5)
  not a number in cond evaluation

  $ ../bin/REPL.exe <<EOF
  > let r = 4*8 in if r < 0 then 8
  (let r = (4 * 8) in (if (r < 0) then 8))
  ()

  $ ../bin/REPL.exe <<EOF
  > let r a = a*a in r (let y = 7)
  (let r = (fun a -> (a * a)) in r (let y = 7))
  Can do binop only with const int

  $ ../bin/REPL.exe <<EOF
  > let t = let r = 8
  (let t = (let r = 8))
  can put only vars and funcs in env

  $ ../bin/REPL.exe <<EOF
  > let r = 5 in let rec f n k = if n > k then n + (f (n-1) k) else k in let y = if r > 0 then -1*r+5 else r-5 in f r y
  (let r = 5 in (let rec f = (fun n -> (fun k -> (if (n > k) then (n + f (n - 1) k) else k))) in (let y = (if (r > 0) then ((-1 * r) + 5) else (r - 5)) in f r y)))
  15

  $ ../bin/REPL.exe --steps=1 --ast <<EOF
  > let x c = c*2 in x g
  Exceed number of redunction posssible: 
  (fun c -> (c * 2))

  $ ../bin/REPL.exe <<EOF
  > let x a b = a + b in let r t = t+t in x (r 10) 7
  (let x = (fun a -> (fun b -> (a + b))) in (let r = (fun t -> (t + t)) in x r 10 7))
  27

  $ ../bin/REPL.exe --steps 100 <<EOF
  > let rec t n= t (n+1) in t 4
  (let rec t = (fun n -> t (n + 1)) in t 4)
  Exceed number of redunction posssible: 
  100
