  $ INTERPETER="../bin/REPL.exe"

  $ cat << EOF | $INTERPETER -infer -expr 
  > 10 + 20 / 4
  inferred: int

  $ cat << EOF | $INTERPETER -infer -expr 
  > 1 == 2
  inferred: bool

  $ cat << EOF | $INTERPETER -infer -expr 
  > (1 + 2, 3 == 4, true)
  inferred: (int, bool, bool)

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
  > (fun x -> x x)
  inferencer error: 'ty0 occurs in ('ty0 -> 'ty1)

  $ cat << EOF | $INTERPETER -infer -expr 
  > (fun f g x -> f)
  inferred: ('ty0 -> ('ty1 -> ('ty2 -> 'ty0)))

  $ cat << EOF | $INTERPETER -infer -expr 
  > (fun f g x -> f x + g x)
  inferred: ('ty0 -> ('ty1 -> ('ty2 -> int)))

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

# weird
  $ cat << EOF | $INTERPETER -infer -expr 
  > if true then fun x -> x else fun y -> y + 1
  inferred: (int -> int)

  $ cat << EOF | $INTERPETER -infer -expr 
  > if true then fun x -> x + 1 else fun y -> y
  inferred: (int -> int)

  $ cat << EOF | $INTERPETER -infer -expr 
  > let f = fun p x -> if p x then 1 else 0 in f
  inferred: (('ty2 -> bool) -> ('ty2 -> int))
#

# value binding
  $ cat << EOF | $INTERPETER -infer -expr 
  > let x = 1 in x
  inferred: int

  $ cat << EOF | $INTERPETER -infer -expr 
  > let (x, y) = (1, 2) in x + y
  inferred: int

  $ cat << EOF | $INTERPETER -infer -expr 
  > let f = fun x -> x + 1 in f 0
  inferred: int

  $ cat << EOF | $INTERPETER -infer -expr 
  > let f = fun x -> x == 0 in f
  inferred: (int -> bool)

  $ cat << EOF | $INTERPETER -infer -expr 
  > let f = fun x -> x == 0 in f 0
  inferred: bool

  $ cat << EOF | $INTERPETER -infer -expr 
  > let f = fun x -> if x then 1 else 0 in f
  inferred: (bool -> int)

  $ cat << EOF | $INTERPETER -infer -expr 
  > let f = fun x -> if x then 1 else 0 in f true
  inferred: int
#

# value binding chain
  $ cat << EOF | $INTERPETER -infer -expr 
  > let x = 1 and y = 2 and z = 3 in x + y + z
  inferred: int

  $ cat << EOF | $INTERPETER -infer -expr 
  > let f = fun p x -> if p x then 1 else 0
  > in f
  inferred: (('ty2 -> bool) -> ('ty2 -> int))

  $ cat << EOF | $INTERPETER -infer -expr 
  > let f = fun p x -> if p x then 1 else 0
  > and p = fun x -> x == 0
  > in f p
  inferred: (int -> int)

  $ cat << EOF | $INTERPETER -infer -expr 
  > let f = fun p x -> if p x then 1 else 0
  > and p = fun x -> x == 0
  > and x = 0
  > in f p x
  inferred: int
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
#

# match with
  $ cat << EOF | $INTERPETER -infer -expr 
  > match 0 with
  > | x -> true
  inferred: bool

  $ cat << EOF | $INTERPETER -infer -expr 
  > match (1, 2) with
  > | x -> x, 3
  inferred: ((int, int), int)

  $ cat << EOF | $INTERPETER -infer -expr 
  > match (1, 2) with
  > | (x, y) -> (1, 2)
  inferred: (int, int)

  $ cat << EOF | $INTERPETER -infer -expr 
  > match 0 with
  > | x -> true
  > | y -> 1
  inferencer error: unification of bool and int failed

  $ cat << EOF | $INTERPETER -infer -expr 
  > match 1 with
  > | x -> x + 1
  inferred: int

  $ cat << EOF | $INTERPETER -infer -expr 
  > match (1, 2) with
  > | (a, b) -> a == b
  inferred: bool

  $ cat << EOF | $INTERPETER -infer -expr 
  > match 1 with
  > | x -> fun y -> x + y
  inferred: (int -> int)
#

# matching should fail
  $ cat << EOF | $INTERPETER -infer -expr 
  > match 0 with
  > | x -> if x then 1 else 0
  inferencer error: unification of bool and int failed

  $ cat << EOF | $INTERPETER -infer -expr 
  > match (1, 2, 3) with
  > | (a, b, c) -> 3
  > | (a, b) -> 2
  > | _ -> 1
  > 
  inferencer error: unification of ('ty0, 'ty1, 'ty2) and ('ty3, 'ty4) failed

  $ cat << EOF | $INTERPETER -infer -expr 
  > match 0 with
  > | x -> if x then 1 else 0
  inferencer error: unification of bool and int failed

  $ cat << EOF | $INTERPETER -infer -expr 
  > match true with
  > | x -> x + 1
  inferencer error: unification of int and bool failed

  $ cat << EOF | $INTERPETER -infer -expr 
  > match true with
  > | (x, y) -> x + y
  inferencer error: unification of bool and ('ty0, 'ty1) failed

  $ cat << EOF | $INTERPETER -infer -expr 
  > match (1, true) with
  > | (x, y) -> x + y
  inferencer error: unification of int and bool failed

  $ cat << EOF | $INTERPETER -infer -expr 
  > match 1 with
  > | x -> fun y -> x + y
  > | x -> fun y -> if y then x else 0 - x
  inferencer error: unification of int and bool failed

  $ cat << EOF | $INTERPETER -infer -expr 
  > match (fun x -> x) with
  > | f -> f 1
  > | f -> f true
  inferencer error: unification of bool and int failed

  $ cat << EOF | $INTERPETER -infer -expr 
  > match (fun x -> x) with
  > | f -> fun y -> f (y + 1)
  > | f -> fun z -> f (z == 1)
  inferencer error: unification of bool and int failed
#

# inferencer errors
  $ cat << EOF | $INTERPETER -infer -expr 
  > x + y
  inferencer error: unbound value: x

  $ cat << EOF | $INTERPETER -infer -expr 
  > (1 == 2) + 3
  inferencer error: unification of bool and int failed

  $ cat << EOF | $INTERPETER -infer -expr 
  > (fun x -> fun y -> x + y) 5 true
  inferencer error: unification of int and bool failed

  $ cat << EOF | $INTERPETER -infer -expr 
  > (fun x -> fun y -> x + y) true false
  inferencer error: unification of int and bool failed

  $ cat << EOF | $INTERPETER -infer -expr 
  > fun x -> if x then true else 0
  inferencer error: unification of bool and int failed

  $ cat << EOF | $INTERPETER -infer -expr 
  > (fun x -> if x then 1 else 0) 1
  inferencer error: unification of bool and int failed

  $ cat << EOF | $INTERPETER -infer -expr 
  > let x = 1 in x == true
  inferencer error: unification of bool and int failed

  $ cat << EOF | $INTERPETER -infer -expr 
  > let f = fun x -> x + 1 in f true
  inferencer error: unification of int and bool failed

  $ cat << EOF | $INTERPETER -infer -expr 
  > let x = if true then 1 else false in x
  inferencer error: unification of int and bool failed
#
