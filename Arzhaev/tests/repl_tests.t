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


Type errors

  $ ../bin/REPL.exe <<EOF
  > 1 + true
  > EOF
  Type error: Ground type mismatch


  $ ../bin/REPL.exe <<EOF
  > let x = 1 in x true
  > EOF
  Type error: Unbound value: x


Parse errors

  $ ../bin/REPL.exe <<EOF
  > let = 10
  > EOF
  Parse error: expected id


Step limit

  $ ../bin/REPL.exe -steps 1 <<EOF
  > let f = fun x -> x + 1
  > f 10
  > EOF
  val f : int -> int
  val f = <fun>
  Error: step limit exceeded


Parsetree dump

  $ ../bin/REPL.exe -dparsetree <<EOF
  > 1 + 2
  > EOF
  AST: (1 + 2)
  - : int
  - : 3
