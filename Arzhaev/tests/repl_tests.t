  $ ../bin/REPL.exe <<EOF
  > 1 + 2
  - : int
  - : 3
  EOF

  $ ../bin/REPL.exe <<EOF
  > let x = 10
  val x : int
  val x = 10
  > x + 5
  - : int
  - : 15
  EOF

  $ ../bin/REPL.exe <<EOF
  > let f = fun x -> x + 1
  val f : int -> int
  val f = <fun>
  > f 5
  - : int
  - : 6
  EOF

  $ ../bin/REPL.exe <<EOF
  > let rec fact = fun n ->
  >   if n = 0 then 1 else n * fact (n - 1)
  val fact : int -> int
  val fact = <fun>
  > fact 5
  - : int
  - : 120
  EOF

  $ ../bin/REPL.exe <<EOF
  > true && false
  - : bool
  - : false
  EOF

  $ ../bin/REPL.exe <<EOF
  > if true then 1 else 2
  - : int
  - : 1
  EOF

Type errors

  $ ../bin/REPL.exe <<EOF
  > 1 + true
  Type error: Ground type mismatch
  EOF

  $ ../bin/REPL.exe <<EOF
  > let x = 1 in x true
  Type error: TODO
  EOF

Parse errors

  $ ../bin/REPL.exe <<EOF
  > let = 10
  Parse error: 
  EOF

Step limit

  $ ../bin/REPL.exe -steps 1 <<EOF
  > let f = fun x -> x + 1
  val f : int -> int
  val f = <fun>
  > f 10
  Error: step limit exceeded
  EOF

Parsetree dump

  $ ../bin/REPL.exe -dparsetree <<EOF
  > 1 + 2
  AST: (EBinOp (Add, (EConst (IConst 1)), (EConst (IConst 2))))
  - : int
  - : 3
  EOF