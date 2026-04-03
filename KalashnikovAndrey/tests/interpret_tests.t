Copyright 2021-2024, Kakadu and contributors
SPDX-License-Identifier: CC0-1.0

Basic arithmetic and conditionals:

  $ ../bin/REPL.exe <<'EOF'
  > 1000
  > false
  > 1 + 2 * 3
  > if 1 then 10 else 20
  > :q
  > EOF
  max steps> ast printer> > Value: 7
  Steps: 995
  > Value: 10
  Steps: 997
  > 

Recursive factorial with `let rec`:

  $ ../bin/REPL.exe <<'EOF'
  > 1000
  > false
  > let rec fact n = if n < 2 then 1 else n * fact (n - 1) in fact 5
  > :q
  > EOF
  max steps> ast printer> > Value: 120
  Steps: 965
  > 

Recursive Fibonacci with `let rec`:

  $ ../bin/REPL.exe <<'EOF'
  > 1000
  > false
  > let rec fib n = if n < 2 then n else fib (n - 1) + fib (n - 2) in fib 7
  > :q
  > EOF
  max steps> ast printer> > Value: 13
  Steps: 734
  > 

Builtin `print` works and user-defined `print` shadows it:

  $ ../bin/REPL.exe <<'EOF'
  > 1000
  > false
  > let x = print 5 in x + 1
  > let print = fun x -> x + 1 in print 5
  > :q
  > EOF
  max steps> ast printer> > 5
  Value: 6
  Steps: 995
  > Value: 6
  Steps: 994
  > 

Step limit stops diverging programs:

  $ ../bin/REPL.exe <<'EOF'
  > 5
  > false
  > let rec loop x = loop x in loop 0
  > :q
  > EOF
  max steps> ast printer> > Error: Out of steps
  Steps: 0
  > 

AST printing mode:

  $ ../bin/REPL.exe <<'EOF'
  > 1000
  > true
  > let x = 5 in x + 1
  > :q
  > EOF
  max steps> ast printer> > Ast: (let x = 5 in (x + 1))
  Value: 6
  Steps: 996
  > 

Parse errors are reported:

  $ ../bin/REPL.exe <<'EOF'
  > 1000
  > false
  > let x = in x
  > :q
  > EOF
  max steps> ast printer> > Parse error: : no more choices
  > 

Runtime errors are reported:

  $ ../bin/REPL.exe <<'EOF'
  > 1000
  > false
  > y
  > 1 / 0
  > 1 2
  > :q
  > EOF
  max steps> ast printer> > Error: Unbound variable 'y'
  Steps: 1000
  > Error: Division by zero
  Steps: 997
  > Error: Type error: Application of non-function
  Steps: 997
  > 

Keyword boundaries stay correct:

  $ ../bin/REPL.exe <<'EOF'
  > 1000
  > true
  > let recx = 1 in recx
  > ifx
  > :q
  > EOF
  max steps> ast printer> > Ast: (let recx = 1 in recx)
  Value: 1
  Steps: 998
  > Error: Unbound variable 'ifx'
  Steps: 1000
  > 
