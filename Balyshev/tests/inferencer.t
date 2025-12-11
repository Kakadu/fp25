  $ INTERPETER="../bin/REPL.exe"

  $ cat << EOF | $INTERPETER -infer -expr 
  > 10 + 20 / 4
  inferred: int

  $ cat << EOF | $INTERPETER -infer -expr 
  > (1 + 2, 3 == 4, true)
  inferred: (int * bool * bool)

  $ cat << EOF | $INTERPETER -infer -expr 
  > fun x -> x
  inferred: ('ty0 -> 'ty0)

  $ cat << EOF | $INTERPETER -infer -expr 
  > (fun x -> x + 1)
  inferred: (int -> int)

  $ cat << EOF | $INTERPETER -infer -expr 
  > (fun x -> x + 1) 2
  inferred: int

  $ cat << EOF | $INTERPETER -infer -expr 
  > fun x y -> x + y
  inferred: (int -> (int -> int))

  $ cat << EOF | $INTERPETER -infer -expr 
  > (fun f -> fun x -> f x)
  inferred: (('ty1 -> 'ty2) -> ('ty1 -> 'ty2))

  $ cat << EOF | $INTERPETER -infer -expr 
  > (fun f g x -> f)
  inferred: ('ty0 -> ('ty1 -> ('ty2 -> 'ty0)))

  $ cat << EOF | $INTERPETER -infer -expr 
  > (fun f g x -> f x + g x)
  inferred: (('ty2 -> int) -> (('ty2 -> int) -> ('ty2 -> int)))

  $ cat << EOF | $INTERPETER -infer -expr 
  > (fun x -> fun y -> x + y)
  inferred: (int -> (int -> int))

  $ cat << EOF | $INTERPETER -infer -expr 
  > (fun x -> fun y -> x + y) 1
  inferred: (int -> int)

  $ cat << EOF | $INTERPETER -infer -expr 
  > (fun x -> fun y -> x + y) 1 2
  inferred: int

# if then else
  $ cat << EOF | $INTERPETER -infer -expr 
  > if true then 1 else 0
  inferred: int

  $ cat << EOF | $INTERPETER -infer -expr 
  > fun x -> if x then 1 else 0
  inferred: (bool -> int)

  $ cat << EOF | $INTERPETER -infer -expr 
  > (fun x -> if x then 1 else 0) true
  inferred: int

  $ cat << EOF | $INTERPETER -infer -expr 
  > fun t -> if t then fun x -> x + 1 else fun y -> y - 1
  inferred: (bool -> (int -> int))

  $ cat << EOF | $INTERPETER -infer -expr 
  > (fun t -> if t then fun x -> x + 1 else fun y -> y - 1) false
  inferred: (int -> int)
#

# value binding
  $ cat << EOF | $INTERPETER -infer -expr 
  > let x = 1 in x
  inferred: int

  $ cat << EOF | $INTERPETER -infer -expr 
  > let (x, y) = (1, 2) in x + y
  inferred: int

  $ cat << EOF | $INTERPETER -infer -expr 
  > let bool_to_int x = if x then 1 else 0 in
  > bool_to_int true
  inferred: int

  $ cat << EOF | $INTERPETER -infer -expr 
  > let apply f x = f x in
  > let is_zero x = x == 0 in
  > let x = 0 in
  > apply is_zero x
  inferred: bool
#

# value binding chain
  $ cat << EOF | $INTERPETER -infer -expr 
  > let x = 1 and y = 2 and z = 3 in x + y + z
  inferred: int

  $ cat << EOF | $INTERPETER -infer -expr 
  > let f p x = if p x then 1 else 0
  > and p x = x < 0
  > in f p
  inferred: (int -> int)
#

# carrying
  $ cat << EOF | $INTERPETER -infer -expr 
  > let a = fun x y -> x + y in a 1
  inferred: (int -> int)

  $ cat << EOF | $INTERPETER -infer -expr 
  > let a = fun x y -> x + y in
  > let b = a 1 in
  > b 2
  inferred: int

  $ cat << EOF | $INTERPETER -infer -expr 
  > let apply = fun f x -> f x in
  > let f = fun x -> x + 1 in
  > apply f
  inferred: (int -> int)

  $ cat << EOF | $INTERPETER -infer -expr 
  > let ite = fun c e1 e2 -> if c then e1 else e2 in
  > let first = ite true
  > and second = ite false
  > in (first 1 2) + (second 3 4)
  inferred: int

  $ cat << EOF | $INTERPETER -infer -expr 
  > let pair a b p = if p then a else b in
  > let first p = p true in
  > let second p = p false in
  > 
  > let int_pair = pair 1 2 in
  > let int_bool_pair = pair 1 true in
  > 
  > (first int_pair, second int_bool_pair)
  inferencer error: unification of int and bool failed
#

# match with should pass
  $ cat << EOF | $INTERPETER -infer -expr 
  > match 0 with
  > | x -> true
  inferred: bool

  $ cat << EOF | $INTERPETER -infer -expr 
  > match 1 with
  > | x -> x + 1
  inferred: int

  $ cat << EOF | $INTERPETER -infer -expr 
  > match (1, 2) with
  > | x -> x, 3
  inferred: ((int * int) * int)

  $ cat << EOF | $INTERPETER -infer -expr 
  > match (1, 2) with
  > | (x, y) -> (1, 2)
  > | x -> x
  > | _ -> (3, 4)
  inferred: (int * int)

  $ cat << EOF | $INTERPETER -infer -expr 
  > match (1, 2) with
  > | (a, b) -> a < b
  > | _ -> if true then true else true
  > | _ -> true
  inferred: bool

  $ cat << EOF | $INTERPETER -infer -expr 
  > match 0 with
  > | _ -> fun x -> x
  > | _ -> fun x -> x + 1
  inferred: (int -> int)

  $ cat << EOF | $INTERPETER -infer -expr
  > match 1 with
  > | x -> x == 0
  > | _ -> true
  inferred: bool

  $ cat << EOF | $INTERPETER -infer -expr
  > match (1, 2, 3) with
  > | (a, b, c) -> a + b + c
  > | _ -> 1
  inferred: int

  $ cat << EOF | $INTERPETER -infer -expr
  > match (fun x -> x + 1) with
  > | f -> f 1
  > | _ -> 1
  inferred: int

  $ cat << EOF | $INTERPETER -infer -expr
  > match ((fun x -> x + 1), (fun x -> x == 0)) with
  > | (f, g) -> f 1, g 0
  > | _ -> 1, true
  inferred: (int * bool)
#

# matching should fail
  $ cat << EOF | $INTERPETER -infer -expr 
  > match 1 with
  > | _ -> 1 
  > | _ -> true
  inferencer error: unification of int and bool failed

  $ cat << EOF | $INTERPETER -infer -expr 
  > match 0 with
  > | x -> if x then 1 else 0
  inferencer error: unification of int and bool failed

  $ cat << EOF | $INTERPETER -infer -expr 
  > match 0 with
  > | x -> true
  > | y -> 1
  inferencer error: unification of bool and int failed

  $ cat << EOF | $INTERPETER -infer -expr 
  > match (1, 2, 3) with
  > | (a, b, c) -> 3
  > | (a, b) -> 2
  > | _ -> 1
  inferencer error: unification of (int * int * int) and ('ty3 * 'ty4) failed

  $ cat << EOF | $INTERPETER -infer -expr 
  > match 0 with
  > | x -> if x then 1 else 0
  inferencer error: unification of int and bool failed

  $ cat << EOF | $INTERPETER -infer -expr 
  > match true with
  > | x -> x + 1
  inferencer error: unification of bool and int failed

  $ cat << EOF | $INTERPETER -infer -expr 
  > match true with
  > | (x, y) -> x + y
  inferencer error: unification of bool and ('ty0 * 'ty1) failed

  $ cat << EOF | $INTERPETER -infer -expr 
  > match (1, true) with
  > | (x, y) -> x + y
  inferencer error: unification of bool and int failed

  $ cat << EOF | $INTERPETER -infer -expr 
  > match 1 with
  > | x -> fun y -> x + y
  > | x -> fun y -> if y then x else 0 - x
  inferencer error: unification of int and bool failed

  $ cat << EOF | $INTERPETER -infer -expr 
  > match (fun x -> x) with
  > | f -> f 1
  > | f -> f true
  inferencer error: unification of int and bool failed

  $ cat << EOF | $INTERPETER -infer -expr 
  > match (fun x -> x) with
  > | f -> fun y -> f (y + 1)
  > | f -> fun z -> f (z == 1)
  inferencer error: unification of int and bool failed

  $ cat << EOF | $INTERPETER -infer -expr
  > match 1 with
  > | x -> x == 0
  > | _ -> 1
  inferencer error: unification of bool and int failed

  $ cat << EOF | $INTERPETER -infer -expr
  > match (1, 2, 3) with
  > | (a, b) -> a + b
  > | _ -> 1
  inferencer error: unification of (int * int * int) and ('ty0 * 'ty1) failed

  $ cat << EOF | $INTERPETER -infer -expr
  > match (fun x -> x + 1) with
  > | f -> f true
  > | _ -> 1
  inferencer error: unification of int and bool failed

  $ cat << EOF | $INTERPETER -infer -expr
  > match ((fun x -> x + 1), (fun x -> x == 0)) with
  > | (f, g) -> f 1, g 0
  > | _ -> 1, 1
  inferencer error: unification of bool and int failed
#

# occurs in
  $ cat << EOF | $INTERPETER -infer -expr 
  > fun x -> x x
  inferencer error: 'ty0 occurs in ('ty0 -> 'ty1)

  $ cat << EOF | $INTERPETER -infer -stru 
  > let f = fun x -> x x
  inferencer error: 'ty0 occurs in ('ty0 -> 'ty1)

  $ cat << EOF | $INTERPETER -infer -expr 
  > (fun f -> f f) (fun f -> f f)
  inferencer error: 'ty0 occurs in ('ty0 -> 'ty1)
#

# should fail
  $ cat << EOF | $INTERPETER -infer -expr 
  > x + y
  inferencer error: unbound value: x

  $ cat << EOF | $INTERPETER -infer -expr 
  > (1 == 2) + 3
  inferencer error: unification of bool and int failed



  $ cat << EOF | $INTERPETER -infer -expr 
  > (fun x -> x + 1) true
  inferencer error: unification of int and bool failed

  $ cat << EOF | $INTERPETER -infer -expr 
  > (fun x -> fun y -> x + y) 5 true
  inferencer error: unification of int and bool failed

  $ cat << EOF | $INTERPETER -infer -expr 
  > if true then 1 else (1, 2)
  inferencer error: unification of int and (int * int) failed

  $ cat << EOF | $INTERPETER -infer -expr 
  > fun x -> if x then true else 0
  inferencer error: unification of bool and int failed

  $ cat << EOF | $INTERPETER -infer -expr 
  > (fun x -> if x then 1 else 0) 1
  inferencer error: unification of bool and int failed

  $ cat << EOF | $INTERPETER -infer -expr 
  > let x = (1, 2) in x == 1
  inferencer error: unification of (int * int) and int failed

  $ cat << EOF | $INTERPETER -infer -expr 
  > let f x = x + 1 in f true
  inferencer error: unification of int and bool failed
#

# let polymorphism
  $ cat << EOF | $INTERPETER -infer -stru 
  > let id x = x
  > let a = id 1
  > let b = id true
  typed structure:
  let id: ('ty0 -> 'ty0) = (fun x -> x)
  
  let a: int = id 1
  
  let b: bool = id true
  $ cat << EOF | $INTERPETER -infer -expr 
  > let id x = x in
  > let a = id 1 in
  > let b = id true in
  > (a, b)
  inferred: (int * bool)

  $ cat << EOF | $INTERPETER -infer -expr 
  > let swap (a, b) = (b, a) in
  > swap (1, 2), swap (true, false)
  inferred: ((int * int) * (bool * bool))

  $ cat << EOF | $INTERPETER -infer -stru
  > let twice f x = f (f x)
  > 
  > let inc x = x + 1
  > let not x = if x then false else true
  > 
  > let two = twice inc 0
  > let bool_id = twice not
  typed structure:
  let twice: (('ty3 -> 'ty3) -> ('ty3 -> 'ty3)) = (fun f -> (fun x -> f f x))
  
  let inc: (int -> int) = (fun x -> (x + 1))
  
  let not: (bool -> bool) = (fun x -> if x then false else true)
  
  let two: int = twice inc 0
  
  let bool_id: (bool -> bool) = twice not

  $ cat << EOF | $INTERPETER -infer -stru 
  > let apply f x = f x
  > let inc = apply (fun x -> x + 1)
  > let not = apply (fun x -> if x then false else true)
  typed structure:
  let apply: (('ty1 -> 'ty2) -> ('ty1 -> 'ty2)) = (fun f -> (fun x -> f x))
  
  let inc: (int -> int) = apply (fun x -> (x + 1))
  
  let not: (bool -> bool) = apply (fun x -> if x then false else true)

  $ cat << EOF | $INTERPETER -infer -expr 
  > let pair a b p = if p then a else b in
  > let first p = p true in
  > let second p = p false in
  > 
  > let int_pair = pair 1 2 in
  > let int_bool_pair = pair false true in
  > 
  > (first int_pair, second int_bool_pair)
  inferred: (int * bool)
#
