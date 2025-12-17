Copyright 2021-2024, Kakadu and contributors
SPDX-License-Identifier: CC0-1.0

Cram tests here. They run and compare program output to the expected output
https://dune.readthedocs.io/en/stable/tests.html#cram-tests
Use `dune promote` after you change things that should runned

If you need to put sample program and use it both in your interpreter and preinstalled one,
you could put it into separate file. Thise will need stanza `(cram (deps demo_input.txt))`
in the dune file

  $ ../bin/REPL.exe <<EOF
  > 5
  Int 5
  5
  

  $ ../bin/REPL.exe <<EOF
  > x
  Var "x"
  Unbound variable: x
  
  $ ../bin/REPL.exe << EOF
  > let h = 1 + 1 in h 
  Let (NonRec, "h", BinOp (Plus, Int 1, Int 1), Some Var "h")
  2
  
  $ ../bin/REPL.exe << EOF
  > let rec fix = fun f ->  (fun x -> f (fun y -> (x x) y))  (fun x -> f (fun y -> (x x) y)) in let fact = fix (fun fact -> fun n -> if n = 0 then 1 else n * fact (n - 1)) in fact 5
  Let (Rec, "fix", Abs ("f", App (Abs ("x", App (Var "f", Abs ("y", App (App (Var "x", Var "x"), Var "y")))), Abs ("x", App (Var "f", Abs ("y", App (App (Var "x", Var "x"), Var "y")))))), Some Let (NonRec, "fact", App (Var "fix", Abs ("fact", Abs ("n", If (BinOp (Equal, Var "n", Int 0), Int 1, Some BinOp (Mult, Var "n", App (Var "fact", BinOp (Minus, Var "n", Int 1))))))), Some App (Var "fact", Int 5)))
  120
  

  $ ../bin/REPL.exe << EOF
  > let x = 7 in let function a b = if x > 4 then x+b else a-b in function 0 1
  Let (NonRec, "x", Int 7, Some Let (NonRec, "function", Abs ("a", Abs ("b", If (BinOp (More, Var "x", Int 4), BinOp (Plus, Var "x", Var "b"), Some BinOp (Minus, Var "a", Var "b")))), Some App (App (Var "function", Int 0), Int 1)))
  8
  


  $ ../bin/REPL.exe << EOF
  > let rec fix f eta = f (fix f) eta in let fact_gen = fun fact -> fun n -> if n = 0 then 1 else n * fact (n - 1) in let fact = fix fact_gen in fact 5
  Let (Rec, "fix", Abs ("f", Abs ("eta", App (App (Var "f", App (Var "fix", Var "f")), Var "eta"))), Some Let (NonRec, "fact_gen", Abs ("fact", Abs ("n", If (BinOp (Equal, Var "n", Int 0), Int 1, Some BinOp (Mult, Var "n", App (Var "fact", BinOp (Minus, Var "n", Int 1)))))), Some Let (NonRec, "fact", App (Var "fix", Var "fact_gen"), Some App (Var "fact", Int 5))))
  120
  
  $ ../bin/REPL.exe << EOF
  > let r = 5 in let rec f n k = if n > k then n + (f (n-1) k) else k in let y = if r > 0 then -1*r+5 else r-5 in f r y
  Let (NonRec, "r", Int 5, Some Let (Rec, "f", Abs ("n", Abs ("k", If (BinOp (More, Var "n", Var "k"), BinOp (Plus, Var "n", App (App (Var "f", BinOp (Minus, Var "n", Int 1)), Var "k")), Some Var "k"))), Some Let (NonRec, "y", If (BinOp (More, Var "r", Int 0), BinOp (Plus, BinOp (Mult, BinOp (Minus, Int 0, Int 1), Var "r"), Int 5), Some BinOp (Minus, Var "r", Int 5)), Some App (App (Var "f", Var "r"), Var "y"))))
  15
  
  $ ../bin/REPL.exe << EOF
  > print_int (10 + 12)
  App (Var "print_int", BinOp (Plus, Int 10, Int 12))
  22
  ()
  
  $ ../bin/REPL.exe << EOF
  > let add = fun x -> fun y -> x + y in print_int (add 3 5) 
  Let (NonRec, "add", Abs ("x", Abs ("y", BinOp (Plus, Var "x", Var "y"))), Some App (Var "print_int", App (App (Var "add", Int 3), Int 5)))
  8
  ()
  

  $ ../bin/REPL.exe << EOF
  > let rec x = 5 in x
  Let (Rec, "x", Int 5, Some Var "x")
  Type error: recursive binding must be a function
  
  $ ../bin/REPL.exe << EOF
  > let f = fun x -> fun y -> fun z -> print_int(x + y + z) in f 1 2 3 
  Let (NonRec, "f", Abs ("x", Abs ("y", Abs ("z", App (Var "print_int", BinOp (Plus, BinOp (Plus, Var "x", Var "y"), Var "z"))))), Some App (App (App (Var "f", Int 1), Int 2), Int 3))
  6
  ()
  
  $ ../bin/REPL.exe << EOF
  > let f = fun x -> fun y -> fun z -> x + y + z in f 1 2 3 
  Let (NonRec, "f", Abs ("x", Abs ("y", Abs ("z", BinOp (Plus, BinOp (Plus, Var "x", Var "y"), Var "z")))), Some App (App (App (Var "f", Int 1), Int 2), Int 3))
  6
  
  $ ../bin/REPL.exe << EOF
  >  let add x y = x + y in let add5 = add 5 in add5 3 + add5 2
  Let (NonRec, "add", Abs ("x", Abs ("y", BinOp (Plus, Var "x", Var "y"))), Some Let (NonRec, "add5", App (Var "add", Int 5), Some BinOp (Plus, App (Var "add5", Int 3), App (Var "add5", Int 2))))
  15
  


  $ ../bin/REPL.exe << EOF
  > let x= 5 in y
  Let (NonRec, "x", Int 5, Some Var "y")
  Unbound variable: y
  

  $ ../bin/REPL.exe << EOF
  > let x i = print_int i in let y = fun z -> if (z > 0) then z else (0-z) in x (y (-5))
  Let (NonRec, "x", Abs ("i", App (Var "print_int", Var "i")), Some Let (NonRec, "y", Abs ("z", If (BinOp (More, Var "z", Int 0), Var "z", Some BinOp (Minus, Int 0, Var "z"))), Some App (Var "x", App (Var "y", BinOp (Minus, Int 0, Int 5)))))
  5
  ()
  

  $ ../bin/REPL.exe << EOF
  > let x = 7 * 8 + 9 in (fun x -> x + x) 5
  Let (NonRec, "x", BinOp (Plus, BinOp (Mult, Int 7, Int 8), Int 9), Some App (Abs ("x", BinOp (Plus, Var "x", Var "x")), Int 5))
  10
  
  $ ../bin/REPL.exe << EOF
  > let r = (fun s k -> s + k) 5 7 in let p = (fun s -> s * 2) ((fun k -> k * 3) 10) in p/2 + r
  Let (NonRec, "r", App (App (Abs ("s", Abs ("k", BinOp (Plus, Var "s", Var "k"))), Int 5), Int 7), Some Let (NonRec, "p", App (Abs ("s", BinOp (Mult, Var "s", Int 2)), App (Abs ("k", BinOp (Mult, Var "k", Int 3)), Int 10)), Some BinOp (Plus, BinOp (Div, Var "p", Int 2), Var "r")))
  42
  
  $ ../bin/REPL.exe << EOF
  > let add x y = x + y in let x = 10 in let y = 20 in add x y
  Let (NonRec, "add", Abs ("x", Abs ("y", BinOp (Plus, Var "x", Var "y"))), Some Let (NonRec, "x", Int 10, Some Let (NonRec, "y", Int 20, Some App (App (Var "add", Var "x"), Var "y"))))
  30
  
  $ ../bin/REPL.exe << EOF
  > if 0 then 1 else 2
  If (Int 0, Int 1, Some Int 2)
  2
  
  $ ../bin/REPL.exe << EOF
  > if 5 then 1 else 2
  If (Int 5, Int 1, Some Int 2)
  1
  
  $ ../bin/REPL.exe << EOF
  > let x = 5 in if x > 3 then x + 1 else x - 1
  Let (NonRec, "x", Int 5, Some If (BinOp (More, Var "x", Int 3), BinOp (Plus, Var "x", Int 1), Some BinOp (Minus, Var "x", Int 1)))
  6
  
  $ ../bin/REPL.exe << EOF
  > let rec factorial n = if n <= 1 then 1 else n * factorial (n - 1) in factorial 6
  Let (Rec, "factorial", Abs ("n", If (BinOp (ELess, Var "n", Int 1), Int 1, Some BinOp (Mult, Var "n", App (Var "factorial", BinOp (Minus, Var "n", Int 1))))), Some App (Var "factorial", Int 6))
  720
  
  $ ../bin/REPL.exe << EOF
  > let rec fib n = if n = 0 then 0 else if n = 1 then 1 else fib (n - 1) + fib (n - 2) in fib 10
  Let (Rec, "fib", Abs ("n", If (BinOp (Equal, Var "n", Int 0), Int 0, Some If (BinOp (Equal, Var "n", Int 1), Int 1, Some BinOp (Plus, App (Var "fib", BinOp (Minus, Var "n", Int 1)), App (Var "fib", BinOp (Minus, Var "n", Int 2)))))), Some App (Var "fib", Int 10))
  55
  
  $ ../bin/REPL.exe << EOF
  > let compose f g = fun x -> f (g x) in let add1 = fun x -> x + 1 in let mul2 = fun x -> x * 2 in let h = compose add1 mul2 in h 5
  Let (NonRec, "compose", Abs ("f", Abs ("g", Abs ("x", App (Var "f", App (Var "g", Var "x"))))), Some Let (NonRec, "add1", Abs ("x", BinOp (Plus, Var "x", Int 1)), Some Let (NonRec, "mul2", Abs ("x", BinOp (Mult, Var "x", Int 2)), Some Let (NonRec, "h", App (App (Var "compose", Var "add1"), Var "mul2"), Some App (Var "h", Int 5)))))
  11
  
  $ ../bin/REPL.exe << EOF
  > let x = 10 in let f = fun y -> x + y in let x = 20 in f 5
  Let (NonRec, "x", Int 10, Some Let (NonRec, "f", Abs ("y", BinOp (Plus, Var "x", Var "y")), Some Let (NonRec, "x", Int 20, Some App (Var "f", Int 5))))
  15
  
  $ ../bin/REPL.exe << EOF
  > let add = fun x -> fun y -> x + y in let add5 = add 5 in print_int (add5 3) in print_int (add5 8)
  Parse error: : end_of_input
  
  $ ../bin/REPL.exe << EOF
  > let max a b = if a > b then a else b in max 7 (max 3 5)
  Let (NonRec, "max", Abs ("a", Abs ("b", If (BinOp (More, Var "a", Var "b"), Var "a", Some Var "b"))), Some App (App (Var "max", Int 7), App (App (Var "max", Int 3), Int 5)))
  7
  
  $ ../bin/REPL.exe << EOF
  > let x = 5 in let y = let x = 10 in x + 1 in x + y
  Let (NonRec, "x", Int 5, Some Let (NonRec, "y", Let (NonRec, "x", Int 10, Some BinOp (Plus, Var "x", Int 1)), Some BinOp (Plus, Var "x", Var "y")))
  16
  
  $ ../bin/REPL.exe << EOF
  > (fun x -> x * x) 9
  App (Abs ("x", BinOp (Mult, Var "x", Var "x")), Int 9)
  81
  
  $ ../bin/REPL.exe << EOF
  > let id = fun x -> x in id id 5
  Let (NonRec, "id", Abs ("x", Var "x"), Some App (App (Var "id", Var "id"), Int 5))
  5
  
  $ ../bin/REPL.exe << EOF
  > let const = fun x -> fun y -> x in const 7 8
  Let (NonRec, "const", Abs ("x", Abs ("y", Var "x")), Some App (App (Var "const", Int 7), Int 8))
  7
  
  $ ../bin/REPL.exe << EOF
  > 1 / 0
  BinOp (Div, Int 1, Int 0)
  Division by zero
  
  $ ../bin/REPL.exe << EOF
  > let rec infinite n = if n = 0 then 0 else infinite n in infinite 5
  Let (Rec, "infinite", Abs ("n", If (BinOp (Equal, Var "n", Int 0), Int 0, Some App (Var "infinite", Var "n"))), Some App (Var "infinite", Int 5))
  Stack overflow
  
  $ ../bin/REPL.exe << EOF
  > let apply_twice f x = f (f x) in let inc = fun x -> x + 1 in apply_twice inc 10
  Let (NonRec, "apply_twice", Abs ("f", Abs ("x", App (Var "f", App (Var "f", Var "x")))), Some Let (NonRec, "inc", Abs ("x", BinOp (Plus, Var "x", Int 1)), Some App (App (Var "apply_twice", Var "inc"), Int 10)))
  12
  
  $ ../bin/REPL.exe << EOF
  > let x = 5 in let f y = x + y in let x = 10 in f 3
  Let (NonRec, "x", Int 5, Some Let (NonRec, "f", Abs ("y", BinOp (Plus, Var "x", Var "y")), Some Let (NonRec, "x", Int 10, Some App (Var "f", Int 3))))
  8
  
