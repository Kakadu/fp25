Fixpoint factorial 

  $ ../bin/REPL.exe <<EOF
  > let rec fix = fun f -> fun x -> f (fix f) x
  > let fac1 = fun self -> fun n -> if n < 2 then 1 else n * self (n - 1)
  > let fac = fun n -> fix fac1 n
  > fac 5
  > EOF
  val fix : forall 'f 'c . (('c -> 'f) -> 'c -> 'f) -> 'c -> 'f
  val fix = <fun>
  val fac1 : (int -> int) -> int -> int
  val fac1 = <fun>
  val fac : int -> int
  val fac = <fun>
  - : int
  - : 120

Direct factorial

  $ ../bin/REPL.exe <<EOF
  > let rec fact = fun n -> if n < 2 then 1 else n * fact (n - 1)
  > fact 6
  > EOF
  val fact : int -> int
  val fact = <fun>
  - : int
  - : 720

Fibonacci naive recursion

  $ ../bin/REPL.exe <<EOF
  > let rec fib = fun n -> if n < 2 then n else fib (n - 1) + fib (n - 2)
  > fib 6
  > EOF
  val fib : int -> int
  val fib = <fun>
  - : int
  - : 8

Fixpoint fibonacci

  $ ../bin/REPL.exe <<EOF
  > let rec fix = fun f -> fun x -> f (fix f) x
  > let fib1 = fun self -> fun n -> if n < 2 then n else self (n - 1) + self (n - 2)
  > let fib = fun n -> fix fib1 n
  > fib 7
  > EOF
  val fix : forall 'f 'c . (('c -> 'f) -> 'c -> 'f) -> 'c -> 'f
  val fix = <fun>
  val fib1 : (int -> int) -> int -> int
  val fib1 = <fun>
  val fib : int -> int
  val fib = <fun>
  - : int
  - : 13

Infinite recursion

  $ ../bin/REPL.exe -steps 50 <<EOF
  > let rec f = fun x -> f x
  > f 0
  > EOF
  val f : forall 'c 'b . 'b -> 'c
  val f = <fun>
  Runtime error: step limit exceeded

Infinite function definition

  $ ../bin/REPL.exe <<EOF
  > let rec f = fun x -> f x
  > EOF
  val f : forall 'c 'b . 'b -> 'c
  val f = <fun>

Basic arithmetic

  $ ../bin/REPL.exe <<EOF
  > 1 + 2
  > EOF
  - : int
  - : 3

Let binding

  $ ../bin/REPL.exe <<EOF
  > let x = 10
  > x + 5
  > EOF
  val x : int
  val x = 10
  - : int
  - : 15

Functions

  $ ../bin/REPL.exe <<EOF
  > let f = fun x -> x + 1
  > f 5
  > EOF
  val f : int -> int
  val f = <fun>
  - : int
  - : 6

Recursion

  $ ../bin/REPL.exe <<EOF
  > let rec fact = fun n -> if n = 0 then 1 else n * fact (n - 1)
  > fact 5
  > EOF
  val fact : int -> int
  val fact = <fun>
  - : int
  - : 120

Boolean expressions

  $ ../bin/REPL.exe <<EOF
  > true && false
  > EOF
  - : bool
  - : false

If expression

  $ ../bin/REPL.exe <<EOF
  > if true then 1 else 2
  > EOF
  - : int
  - : 1

Type errors - mismatch

  $ ../bin/REPL.exe <<EOF
  > 1 + true
  > EOF
  Type error: Type mismatch: int vs bool

Type errors - unbound variable

  $ ../bin/REPL.exe <<EOF
  > let x = 1 in y
  > EOF
  Type error: unbound value: y

Occurs check

  $ ../bin/REPL.exe <<EOF
  > let f = fun x -> x x
  > EOF
  Type error: occurs check failed: 'a in 'a -> 'b

Parse error - let

  $ ../bin/REPL.exe <<EOF
  > let = 10
  > EOF
  Parse error: syntax error

Parse error - operator

  $ ../bin/REPL.exe <<EOF
  > 1 + 
  > EOF
  Parse error: syntax error

Parse error - syntax

  $ ../bin/REPL.exe <<EOF
  > if true then 1
  > EOF
  Parse error: syntax error

Runtime error - division by zero

  $ ../bin/REPL.exe <<EOF
  > 1 / 0
  > EOF
  Runtime error: division by zero

Runtime error - not a function

  $ ../bin/REPL.exe <<EOF
  > 5 10
  > EOF
  Type error: Type mismatch: int vs int -> 'a

Runtime error - unbound

  $ ../bin/REPL.exe <<EOF
  > x
  > EOF
  Type error: unbound value: x

Step limit

  $ ../bin/REPL.exe -steps 1 <<EOF
  > let f = fun x -> x + 1
  > f 10
  > EOF
  val f : int -> int
  val f = <fun>
  Runtime error: step limit exceeded

Composition

  $ ../bin/REPL.exe <<EOF
  > let f = fun x -> x
  > let g = fun y -> f y
  > g 10
  > EOF
  val f : forall 'a . 'a -> 'a
  val f = <fun>
  val g : forall 'c . 'c -> 'c
  val g = <fun>
  - : int
  - : 10

Shadowing

  $ ../bin/REPL.exe <<EOF
  > let x = 10
  > let f = fun x -> x + 1
  > f 5
  > x
  > EOF
  val x : int
  val x = 10
  val f : int -> int
  val f = <fun>
  - : int
  - : 6
  - : int
  - : 10

Higher order function

  $ ../bin/REPL.exe <<EOF
  > let apply = fun f -> fun x -> f x
  > let inc = fun x -> x + 1
  > apply inc 10
  > EOF
  val apply : forall 'c 'b . ('b -> 'c) -> 'b -> 'c
  val apply = <fun>
  val inc : int -> int
  val inc = <fun>
  - : int
  - : 11

Recursion + HOF

  $ ../bin/REPL.exe <<EOF
  > let rec map = fun f -> fun x -> if x = 0 then 0 else f x
  > let inc = fun x -> x + 1
  > map inc 10
  > EOF
  val map : (int -> int) -> int -> int
  val map = <fun>
  val inc : int -> int
  val inc = <fun>
  - : int
  - : 11
