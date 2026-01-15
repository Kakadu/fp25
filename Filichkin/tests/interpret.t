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
  Type: int
  5
  
  $ ../bin/REPL.exe <<EOF
  > x
  Var "x"
  Type error: unbound variable x
  $ ../bin/REPL.exe << EOF
  > let h = 1 + 1 in h 
  Let (NonRec, "Var h", BinOp (Plus, Int 1, Int 1), Some Var "h")
  Type: int
  2
  
  $ ../bin/REPL.exe << EOF
  > let rec fix = fun f ->  (fun x -> f (fun y -> (x x) y))  (fun x -> f (fun y -> (x x) y)) in let fact = fix (fun fact -> fun n -> if n = 0 then 1 else n * fact (n - 1)) in fact 5
  Let (Rec, "Var fix", Abs ("f", App (Abs ("x", App (Var "f", Abs ("y", App (App (Var "x", Var "x"), Var "y")))), Abs ("x", App (Var "f", Abs ("y", App (App (Var "x", Var "x"), Var "y")))))), Some Let (NonRec, "Var fact", App (Var "fix", Abs ("fact", Abs ("n", If (BinOp (Equal, Var "n", Int 0), Int 1, BinOp (Mult, Var "n", App (Var "fact", BinOp (Minus, Var "n", Int 1))))))), Some App (Var "fact", Int 5)))
  Type error: recursive type

  $ ../bin/REPL.exe << EOF
  > let x = 7 in let function a b = if x > 4 then x+b else a-b in function 0 1
  Let (NonRec, "Var x", Int 7, Some Let (NonRec, "Var function", Abs ("a", Abs ("b", If (BinOp (More, Var "x", Int 4), BinOp (Plus, Var "x", Var "b"), BinOp (Minus, Var "a", Var "b")))), Some App (App (Var "function", Int 0), Int 1)))
  Type: int
  8
  

  $ ../bin/REPL.exe << EOF
  > let rec fix f eta = f (fix f) eta in let fact_gen = fun fact -> fun n -> if n = 0 then 1 else n * fact (n - 1) in let fact = fix fact_gen in fact 5
  Let (Rec, "Var fix", Abs ("f", Abs ("eta", App (App (Var "f", App (Var "fix", Var "f")), Var "eta"))), Some Let (NonRec, "Var fact_gen", Abs ("fact", Abs ("n", If (BinOp (Equal, Var "n", Int 0), Int 1, BinOp (Mult, Var "n", App (Var "fact", BinOp (Minus, Var "n", Int 1)))))), Some Let (NonRec, "Var fact", App (Var "fix", Var "fact_gen"), Some App (Var "fact", Int 5))))
  Type error: recursive type
  $ ../bin/REPL.exe << EOF
  > let r = 5 in let rec f n k = if n > k then n + (f (n- 1) k) else k in let y = if r > 0 then -1*r+5 else r-5 in f r y
  Let (NonRec, "Var r", Int 5, Some Let (Rec, "Var f", Abs ("n", Abs ("k", If (BinOp (More, Var "n", Var "k"), BinOp (Plus, Var "n", App (App (Var "f", BinOp (Minus, Var "n", Int 1)), Var "k")), Var "k"))), Some Let (NonRec, "Var y", If (BinOp (More, Var "r", Int 0), BinOp (Plus, BinOp (Mult, UnOp (Neg, Int 1), Var "r"), Int 5), BinOp (Minus, Var "r", Int 5)), Some App (App (Var "f", Var "r"), Var "y"))))
  Type: int
  15
  
  $ ../bin/REPL.exe << EOF
  > print_int (10 + 12)
  App (Var "print_int", BinOp (Plus, Int 10, Int 12))
  Type: unit
  22
  ()
  
  $ ../bin/REPL.exe << EOF
  > let add = fun x -> fun y -> x + y in print_int (add 3 5) 
  Let (NonRec, "Var add", Abs ("x", Abs ("y", BinOp (Plus, Var "x", Var "y"))), Some App (Var "print_int", App (App (Var "add", Int 3), Int 5)))
  Type: unit
  8
  ()
  

  $ ../bin/REPL.exe << EOF
  > let rec x = 5 in x
  Let (Rec, "Var x", Int 5, Some Var "x")
  Type: int
  Type error: recursive let-in must bind function
  $ ../bin/REPL.exe << EOF
  > let f = fun x -> fun y -> fun z -> print_int(x + y + z) in f 1 2 3 
  Let (NonRec, "Var f", Abs ("x", Abs ("y", Abs ("z", App (Var "print_int", BinOp (Plus, BinOp (Plus, Var "x", Var "y"), Var "z"))))), Some App (App (App (Var "f", Int 1), Int 2), Int 3))
  Type: unit
  6
  ()
  
  $ ../bin/REPL.exe << EOF
  > let f = fun x -> fun y -> fun z -> x + y + z in f 1 2 3 
  Let (NonRec, "Var f", Abs ("x", Abs ("y", Abs ("z", BinOp (Plus, BinOp (Plus, Var "x", Var "y"), Var "z")))), Some App (App (App (Var "f", Int 1), Int 2), Int 3))
  Type: int
  6
  
  $ ../bin/REPL.exe << EOF
  >  let add x y = x + y in let add5 = add 5 in add5 3 + add5 2
  Let (NonRec, "Var add", Abs ("x", Abs ("y", BinOp (Plus, Var "x", Var "y"))), Some Let (NonRec, "Var add5", App (Var "add", Int 5), Some BinOp (Plus, App (Var "add5", Int 3), App (Var "add5", Int 2))))
  Type: int
  15
  


  $ ../bin/REPL.exe << EOF
  > let x= 5 in y
  Let (NonRec, "Var x", Int 5, Some Var "y")
  Type error: unbound variable y

  $ ../bin/REPL.exe << EOF
  > let x i = print_int i in let y = fun z -> if (z > 0) then z else (0- z) in x (y (-5))
  Let (NonRec, "Var x", Abs ("i", App (Var "print_int", Var "i")), Some Let (NonRec, "Var y", Abs ("z", If (BinOp (More, Var "z", Int 0), Var "z", BinOp (Minus, Int 0, Var "z"))), Some App (Var "x", App (Var "y", UnOp (Neg, Int 5)))))
  Type: unit
  5
  ()
  

  $ ../bin/REPL.exe << EOF
  > let x = 7 * 8 + 9 in (fun x -> x + x) 5
  Let (NonRec, "Var x", BinOp (Plus, BinOp (Mult, Int 7, Int 8), Int 9), Some App (Abs ("x", BinOp (Plus, Var "x", Var "x")), Int 5))
  Type: int
  10
  
  $ ../bin/REPL.exe << EOF
  > let r = (fun s k -> s + k) 5 7 in let p = (fun s -> s * 2) ((fun k -> k * 3) 10) in p/2 + r
  Let (NonRec, "Var r", App (App (Abs ("s", Abs ("k", BinOp (Plus, Var "s", Var "k"))), Int 5), Int 7), Some Let (NonRec, "Var p", App (Abs ("s", BinOp (Mult, Var "s", Int 2)), App (Abs ("k", BinOp (Mult, Var "k", Int 3)), Int 10)), Some BinOp (Plus, BinOp (Div, Var "p", Int 2), Var "r")))
  Type: int
  42
  
  $ ../bin/REPL.exe << EOF
  > let add x y = x + y in let x = 10 in let y = 20 in add x y
  Let (NonRec, "Var add", Abs ("x", Abs ("y", BinOp (Plus, Var "x", Var "y"))), Some Let (NonRec, "Var x", Int 10, Some Let (NonRec, "Var y", Int 20, Some App (App (Var "add", Var "x"), Var "y"))))
  Type: int
  30
  
  $ ../bin/REPL.exe << EOF
  > if 5 <> 4 then 1 else 2
  If (BinOp (NotEqual, Int 5, Int 4), Int 1, Int 2)
  Type: int
  1
  
  $ ../bin/REPL.exe << EOF
  > let x = 5 in if x > 3 then x + 1 else x - 1
  Let (NonRec, "Var x", Int 5, Some If (BinOp (More, Var "x", Int 3), BinOp (Plus, Var "x", Int 1), BinOp (Minus, Var "x", Int 1)))
  Type: int
  6
  
  $ ../bin/REPL.exe << EOF
  > let rec factorial n = if n <= 1 then 1 else n * factorial (n - 1) in factorial 6
  Let (Rec, "Var factorial", Abs ("n", If (BinOp (ELess, Var "n", Int 1), Int 1, BinOp (Mult, Var "n", App (Var "factorial", BinOp (Minus, Var "n", Int 1))))), Some App (Var "factorial", Int 6))
  Type: int
  720
  
  $ ../bin/REPL.exe << EOF
  > let rec fib n = if n = 0 then 0 else if n = 1 then 1 else fib (n - 1) + fib (n - 2) in fib 10
  Let (Rec, "Var fib", Abs ("n", If (BinOp (Equal, Var "n", Int 0), Int 0, If (BinOp (Equal, Var "n", Int 1), Int 1, BinOp (Plus, App (Var "fib", BinOp (Minus, Var "n", Int 1)), App (Var "fib", BinOp (Minus, Var "n", Int 2)))))), Some App (Var "fib", Int 10))
  Type: int
  55
  
  $ ../bin/REPL.exe << EOF
  > let compose f g = fun x -> f (g x) in let add1 = fun x -> x + 1 in let mul2 = fun x -> x * 2 in let h = compose add1 mul2 in h 5
  Let (NonRec, "Var compose", Abs ("f", Abs ("g", Abs ("x", App (Var "f", App (Var "g", Var "x"))))), Some Let (NonRec, "Var add1", Abs ("x", BinOp (Plus, Var "x", Int 1)), Some Let (NonRec, "Var mul2", Abs ("x", BinOp (Mult, Var "x", Int 2)), Some Let (NonRec, "Var h", App (App (Var "compose", Var "add1"), Var "mul2"), Some App (Var "h", Int 5)))))
  Type: int
  11
  
  $ ../bin/REPL.exe << EOF
  > let x = 10 in let f = fun y -> x + y in let x = 20 in f 5
  Let (NonRec, "Var x", Int 10, Some Let (NonRec, "Var f", Abs ("y", BinOp (Plus, Var "x", Var "y")), Some Let (NonRec, "Var x", Int 20, Some App (Var "f", Int 5))))
  Type: int
  15
  
  $ ../bin/REPL.exe << EOF
  > let add = fun x -> fun y -> x + y in let add5 = add 5 in print_int (add5 3) in print_int (add5 8)
  Parse error: : end_of_input
  $ ../bin/REPL.exe << EOF
  > let max a b = if a > b then a else b in max 7 (max 3 5)
  Let (NonRec, "Var max", Abs ("a", Abs ("b", If (BinOp (More, Var "a", Var "b"), Var "a", Var "b"))), Some App (App (Var "max", Int 7), App (App (Var "max", Int 3), Int 5)))
  Type: int
  7
  
  $ ../bin/REPL.exe << EOF
  > let x = 5 in let y = let x = 10 in x + 1 in x + y
  Let (NonRec, "Var x", Int 5, Some Let (NonRec, "Var y", Let (NonRec, "Var x", Int 10, Some BinOp (Plus, Var "x", Int 1)), Some BinOp (Plus, Var "x", Var "y")))
  Type: int
  16
  
  $ ../bin/REPL.exe << EOF
  > (fun x -> x * x) 9
  App (Abs ("x", BinOp (Mult, Var "x", Var "x")), Int 9)
  Type: int
  81
  
  $ ../bin/REPL.exe << EOF
  > let id = fun x -> x in id id 5
  Let (NonRec, "Var id", Abs ("x", Var "x"), Some App (App (Var "id", Var "id"), Int 5))
  Type: int
  5
  
  $ ../bin/REPL.exe << EOF
  > let const = fun x -> fun y -> x in const 7 8
  Let (NonRec, "Var const", Abs ("x", Abs ("y", Var "x")), Some App (App (Var "const", Int 7), Int 8))
  Type: int
  7
  
  $ ../bin/REPL.exe << EOF
  > 1 / 0
  BinOp (Div, Int 1, Int 0)
  Type: int
  Division by zero
  $ ../bin/REPL.exe << EOF
  > let rec infinite n = if n = 0 then 0 else infinite n in infinite 5
  Let (Rec, "Var infinite", Abs ("n", If (BinOp (Equal, Var "n", Int 0), Int 0, App (Var "infinite", Var "n"))), Some App (Var "infinite", Int 5))
  Type: int
  Step count is zero
  $ ../bin/REPL.exe << EOF
  > let apply_twice f x = f (f x) in let inc = fun x -> x + 1 in apply_twice inc 10
  Let (NonRec, "Var apply_twice", Abs ("f", Abs ("x", App (Var "f", App (Var "f", Var "x")))), Some Let (NonRec, "Var inc", Abs ("x", BinOp (Plus, Var "x", Int 1)), Some App (App (Var "apply_twice", Var "inc"), Int 10)))
  Type: int
  12
  
  $ ../bin/REPL.exe << EOF
  > let x = 5 in let f y = x + y in let x = 10 in f 3
  Let (NonRec, "Var x", Int 5, Some Let (NonRec, "Var f", Abs ("y", BinOp (Plus, Var "x", Var "y")), Some Let (NonRec, "Var x", Int 10, Some App (Var "f", Int 3))))
  Type: int
  8
  
  $ ../bin/REPL.exe << EOF
  > true && false
  BinOp (And, Bool (true), Bool (false))
  Type: bool
  false
  
  $ ../bin/REPL.exe << EOF
  > true || false
  BinOp (Or, Bool (true), Bool (false))
  Type: bool
  true
  
  $ ../bin/REPL.exe << EOF
  > not true
  UnOp (Not, Bool (true))
  Type: bool
  false
  
  $ ../bin/REPL.exe << EOF
  > not false
  UnOp (Not, Bool (false))
  Type: bool
  true
  
  $ ../bin/REPL.exe << EOF
  > 5 + true
  BinOp (Plus, Int 5, Bool (true))
  Type error: type mismatch
  $ ../bin/REPL.exe << EOF
  > true + false
  BinOp (Plus, Bool (true), Bool (false))
  Type error: type mismatch
  $ ../bin/REPL.exe << EOF
  > (1, 2, 3)
  Tuple [Int 1; Int 2; Int 3]
  Type: (int * int * int)
  (1, 2, 3)
  
  $ ../bin/REPL.exe << EOF
  > let (x, y) = (5, 6) in x + y
  Let (NonRec, "PTuple [Var x; Var y]", Tuple [Int 5; Int 6], Some BinOp (Plus, Var "x", Var "y"))
  Type: int
  11
  
  $ ../bin/REPL.exe << EOF
  > let (a, (b, c)) = (1, (2, 3)) in a + b + c
  Let (NonRec, "PTuple [Var a; PTuple [Var b; Var c]]", Tuple [Int 1; Tuple [Int 2; Int 3]], Some BinOp (Plus, BinOp (Plus, Var "a", Var "b"), Var "c"))
  Type: int
  6
  
  $ ../bin/REPL.exe << EOF
  > let (x, y) = 5 in x
  Let (NonRec, "PTuple [Var x; Var y]", Int 5, Some Var "x")
  Type error: pattern matching error: type mismatch
  $ ../bin/REPL.exe << EOF
  > let rec mul x y = if x = 0 then 0 else y + mul (x-1) y in mul 3 4
  Let (Rec, "Var mul", Abs ("x", Abs ("y", If (BinOp (Equal, Var "x", Int 0), Int 0, BinOp (Plus, Var "y", App (App (Var "mul", BinOp (Minus, Var "x", Int 1)), Var "y"))))), Some App (App (Var "mul", Int 3), Int 4))
  Type: int
  12
  
  $ ../bin/REPL.exe << EOF
  > let rec sum_pair (x, y) = x + y in sum_pair (3, 4)
  Let (Rec, "Var sum_pair", Abs ("(x, y)", BinOp (Plus, Var "x", Var "y")), Some App (Var "sum_pair", Tuple [Int 3; Int 4]))
  Type: int
  7
  
  $ ../bin/REPL.exe << EOF
  > let rec f n = f n in f 5
  Let (Rec, "Var f", Abs ("n", App (Var "f", Var "n")), Some App (Var "f", Int 5))
  Type error: recursive type
  $ ../bin/REPL.exe << EOF
  > if true then 1 else false
  If (Bool (true), Int 1, Bool (false))
  Type error: type mismatch
  $ ../bin/REPL.exe << EOF
  > let x = if true then 1 else 2 in x + 3
  Let (NonRec, "Var x", If (Bool (true), Int 1, Int 2), Some BinOp (Plus, Var "x", Int 3))
  Type: int
  4
  
  $ ../bin/REPL.exe << EOF
  > let x = if false then 1 else 2 in x * 3
  Let (NonRec, "Var x", If (Bool (false), Int 1, Int 2), Some BinOp (Mult, Var "x", Int 3))
  Type: int
  6
  
  $ ../bin/REPL.exe << EOF
  > let f = fun (x, y) -> x + y in f (3, 4)
  Let (NonRec, "Var f", Abs ("(x, y)", BinOp (Plus, Var "x", Var "y")), Some App (Var "f", Tuple [Int 3; Int 4]))
  Type: int
  7
  
  $ ../bin/REPL.exe << EOF
  > 10 >= 5
  BinOp (EMore, Int 10, Int 5)
  Type: bool
  true
  
  $ ../bin/REPL.exe << EOF
  > 5 <= 5
  BinOp (ELess, Int 5, Int 5)
  Type: bool
  true
  
  $ ../bin/REPL.exe << EOF
  > 5 < 3
  BinOp (Less, Int 5, Int 3)
  Type: bool
  false
  
  $ ../bin/REPL.exe << EOF
  > 5 > 3
  BinOp (More, Int 5, Int 3)
  Type: bool
  true
  
  $ ../bin/REPL.exe << EOF
  > 5 = 5
  BinOp (Equal, Int 5, Int 5)
  Type: bool
  true
  
  $ ../bin/REPL.exe << EOF
  > 5 <> 3
  BinOp (NotEqual, Int 5, Int 3)
  Type: bool
  true
  
  $ ../bin/REPL.exe << EOF
  > let x = 5 in let y = x * 2 in let z = y + 3 in z
  Let (NonRec, "Var x", Int 5, Some Let (NonRec, "Var y", BinOp (Mult, Var "x", Int 2), Some Let (NonRec, "Var z", BinOp (Plus, Var "y", Int 3), Some Var "z")))
  Type: int
  13
  
  $ ../bin/REPL.exe << EOF
  > let rec pow x n = if n = 0 then 1 else x * pow x (n-1) in pow 2 5
  Let (Rec, "Var pow", Abs ("x", Abs ("n", If (BinOp (Equal, Var "n", Int 0), Int 1, BinOp (Mult, Var "x", App (App (Var "pow", Var "x"), BinOp (Minus, Var "n", Int 1)))))), Some App (App (Var "pow", Int 2), Int 5))
  Type: int
  32
  
  $ ../bin/REPL.exe << EOF
  > let x = (1, (2, 3), 4) in x
  Let (NonRec, "Var x", Tuple [Int 1; Tuple [Int 2; Int 3]; Int 4], Some Var "x")
  Type: (int * (int * int) * int)
  (1, (2, 3), 4)
  
