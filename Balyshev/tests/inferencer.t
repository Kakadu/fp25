  $ infer () { ../bin/REPL.exe -infer "$@"; }

  $ infer -expr << EOF
  > 10 + 20 / 4
  inferred: int

  $ infer -expr << EOF
  > (1 + 2, 3 == 4, true)
  inferred: int * bool * bool

  $ infer -expr << EOF
  > fun x -> x
  inferred: 'ty0 -> 'ty0

  $ infer -expr << EOF
  > (fun x -> x + 1)
  inferred: int -> int

  $ infer -expr << EOF
  > (fun x -> x + 1) 2
  inferred: int

  $ infer -expr << EOF
  > fun x y -> x + y
  inferred: int -> int -> int

  $ infer -expr << EOF
  > (fun f -> fun x -> f x)
  inferred: ('ty1 -> 'ty2) -> 'ty1 -> 'ty2

  $ infer -expr << EOF
  > (fun a b c -> a)
  inferred: 'ty0 -> 'ty1 -> 'ty2 -> 'ty0

  $ infer -expr << EOF
  > (fun f g x -> f x + g x)
  inferred: ('ty2 -> int) -> ('ty2 -> int) -> 'ty2 -> int

  $ infer -expr << EOF
  > (fun x -> fun y -> x + y)
  inferred: int -> int -> int

  $ infer -expr << EOF
  > (fun x -> fun y -> x + y) 1
  inferred: int -> int

  $ infer -expr << EOF
  > (fun x -> fun y -> x + y) 1 2
  inferred: int

# if then else
  $ infer -expr << EOF
  > if true then 1 else 0
  inferred: int

  $ infer -expr << EOF
  > fun x -> if x then 1 else 0
  inferred: bool -> int

  $ infer -expr << EOF
  > (fun x -> if x then 1 else 0) true
  inferred: int

  $ infer -expr << EOF
  > fun t -> if t then fun x -> x + 1 else fun y -> y - 1
  inferred: bool -> int -> int

  $ infer -expr << EOF
  > (fun t -> if t then fun x -> x + 1 else fun y -> y - 1) false
  inferred: int -> int
#

# value binding
  $ infer -expr << EOF
  > let x = 1 in x
  inferred: int

  $ infer -expr << EOF
  > let (x, y) = (1, 2) in x + y
  inferred: int

  $ infer -expr << EOF
  > let bool_to_int x = if x then 1 else 0 in
  > bool_to_int true
  inferred: int

  $ infer -expr << EOF
  > let apply f x = f x in
  > let is_zero x = x == 0 in
  > let x = 0 in
  > apply is_zero x
  inferred: bool
#

# value binding chain
  $ infer -expr << EOF
  > let x = 1 and y = 2 and z = 3 in x + y + z
  inferred: int

  $ infer -expr << EOF
  > let f p x = if p x then 1 else 0
  > and p x = x < 0
  > in f p
  inferred: int -> int
#

# carrying
  $ infer -expr << EOF
  > let a = fun x y -> x + y in a 1
  inferred: int -> int

  $ infer -expr << EOF
  > let a = fun x y -> x + y in
  > let b = a 1 in
  > b 2
  inferred: int

  $ infer -expr << EOF
  > let apply = fun f x -> f x in
  > let f = fun x -> x + 1 in
  > apply f
  inferred: int -> int

  $ infer -expr << EOF
  > let ite = fun c e1 e2 -> if c then e1 else e2 in
  > let first = ite true
  > and second = ite false
  > in (first 1 2) + (second 3 4)
  inferred: int

  $ infer -expr << EOF
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
  $ infer -expr << EOF
  > match 0 with
  > | x -> true
  inferred: bool

  $ infer -expr << EOF
  > match 1 with
  > | x -> x + 1
  inferred: int

  $ infer -expr << EOF
  > match (1, 2) with
  > | x -> x, 3
  inferred: (int * int) * int

  $ infer -expr << EOF
  > match (1, 2) with
  > | (x, y) -> (1, 2)
  > | x -> x
  > | _ -> (3, 4)
  inferred: int * int

  $ infer -expr << EOF
  > match (1, 2) with
  > | (a, b) -> a < b
  > | _ -> if true then true else true
  > | _ -> true
  inferred: bool

  $ infer -expr << EOF
  > match 0 with
  > | _ -> fun x -> x
  > | _ -> fun x -> x + 1
  inferred: int -> int

  $ infer -expr << EOF
  > match 1 with
  > | x -> x == 0
  > | _ -> true
  inferred: bool

  $ infer -expr << EOF
  > match (1, 2, 3) with
  > | (a, b, c) -> a + b + c
  > | _ -> 1
  inferred: int

  $ infer -expr << EOF
  > match (fun x -> x + 1) with
  > | f -> f 1
  > | _ -> 1
  inferred: int

  $ infer -expr << EOF
  > match ((fun x -> x + 1), (fun x -> x == 0)) with
  > | (f, g) -> f 1, g 0
  > | _ -> 1, true
  inferred: int * bool
#

# matching should fail
  $ infer -expr << EOF
  > match 1 with
  > | _ -> 1 
  > | _ -> true
  inferencer error: unification of int and bool failed

  $ infer -expr << EOF
  > match 0 with
  > | x -> if x then 1 else 0
  inferencer error: unification of int and bool failed

  $ infer -expr << EOF
  > match 0 with
  > | x -> true
  > | y -> 1
  inferencer error: unification of bool and int failed

  $ infer -expr << EOF
  > match (1, 2, 3) with
  > | (a, b, c) -> 3
  > | (a, b) -> 2
  > | _ -> 1
  inferencer error: unification of int * int * int and 'ty3 * 'ty4 failed

  $ infer -expr << EOF
  > match 0 with
  > | x -> if x then 1 else 0
  inferencer error: unification of int and bool failed

  $ infer -expr << EOF
  > match true with
  > | x -> x + 1
  inferencer error: unification of bool and int failed

  $ infer -expr << EOF
  > match true with
  > | (x, y) -> x + y
  inferencer error: unification of bool and 'ty0 * 'ty1 failed

  $ infer -expr << EOF
  > match (1, true) with
  > | (x, y) -> x + y
  inferencer error: unification of bool and int failed

  $ infer -expr << EOF
  > match 1 with
  > | x -> fun y -> x + y
  > | x -> fun y -> if y then x else 0 - x
  inferencer error: unification of int and bool failed

  $ infer -expr << EOF
  > match (fun x -> x) with
  > | f -> f 1
  > | f -> f true
  inferencer error: unification of int and bool failed

  $ infer -expr << EOF
  > match (fun x -> x) with
  > | f -> fun y -> f (y + 1)
  > | f -> fun z -> f (z == 1)
  inferencer error: unification of int and bool failed

  $ infer -expr << EOF
  > match 1 with
  > | x -> x == 0
  > | _ -> 1
  inferencer error: unification of bool and int failed

  $ infer -expr << EOF
  > match (1, 2, 3) with
  > | (a, b) -> a + b
  > | _ -> 1
  inferencer error: unification of int * int * int and 'ty0 * 'ty1 failed

  $ infer -expr << EOF
  > match (fun x -> x + 1) with
  > | f -> f true
  > | _ -> 1
  inferencer error: unification of int and bool failed

  $ infer -expr << EOF
  > match ((fun x -> x + 1), (fun x -> x == 0)) with
  > | (f, g) -> f 1, g 0
  > | _ -> 1, 1
  inferencer error: unification of bool and int failed
#

# occurs check should fail
  $ infer -expr << EOF
  > fun x -> x x
  inferencer error: 'ty0 occurs in 'ty0 -> 'ty1

  $ infer -stru << EOF
  > let f = fun x -> x x
  inferencer error: 'ty0 occurs in 'ty0 -> 'ty1

  $ infer -expr << EOF
  > (fun f -> f f) (fun f -> f f)
  inferencer error: 'ty0 occurs in 'ty0 -> 'ty1

  $ infer -stru << EOF
  > let rec loop x = loop x, 1
  inferencer error: 'ty2 occurs in 'ty2 * int
#

# should fail
  $ infer -expr << EOF
  > x + y
  inferencer error: unbound value: x

  $ infer -expr << EOF
  > (1 == 2) + 3
  inferencer error: unification of bool and int failed

  $ infer -expr << EOF
  > (fun x -> x + 1) true
  inferencer error: unification of int and bool failed

  $ infer -expr << EOF
  > (fun x -> fun y -> x + y) 5 true
  inferencer error: unification of int and bool failed

  $ infer -expr << EOF
  > if true then 1 else (1, 2)
  inferencer error: unification of int and int * int failed

  $ infer -expr << EOF
  > fun x -> if x then true else 0
  inferencer error: unification of bool and int failed

  $ infer -expr << EOF
  > (fun x -> if x then 1 else 0) 1
  inferencer error: unification of bool and int failed

  $ infer -expr << EOF
  > let x = (1, 2) in x == 1
  inferencer error: unification of int * int and int failed

  $ infer -expr << EOF
  > let f x = x + 1 in f true
  inferencer error: unification of int and bool failed
#

# let polymorphism
  $ infer -stru << EOF
  > let id x = x
  > let a = id 1
  > let b = id true
  typed structure:
  let id: 'ty0 -> 'ty0 = fun x -> x
  
  let a: int = id 1
  
  let b: bool = id true
  $ infer -expr << EOF
  > let id x = x in
  > let a = id 1 in
  > let b = id true in
  > (a, b)
  inferred: int * bool

  $ infer -expr << EOF
  > let swap (a, b) = (b, a) in
  > swap (1, 2), swap (true, false)
  inferred: (int * int) * (bool * bool)

  $ infer -stru << EOF
  > let twice f x = f (f x)
  > 
  > let inc x = x + 1
  > let not x = if x then false else true
  > 
  > let two = twice inc 0
  > let bool_id = twice not
  typed structure:
  let twice: ('ty3 -> 'ty3) -> 'ty3 -> 'ty3 = fun f -> fun x -> f (f x)
  
  let inc: int -> int = fun x -> x + 1
  
  let not: bool -> bool = fun x -> if x then false else true
  
  let two: int = (twice inc) 0
  
  let bool_id: bool -> bool = twice not

  $ infer -stru << EOF
  > let apply f x = f x
  > let inc = apply (fun x -> x + 1)
  > let not = apply (fun x -> if x then false else true)
  typed structure:
  let apply: ('ty1 -> 'ty2) -> 'ty1 -> 'ty2 = fun f -> fun x -> f x
  
  let inc: int -> int = apply (fun x -> x + 1)
  
  let not: bool -> bool = apply (fun x -> if x then false else true)

  $ infer -expr << EOF
  > let pair a b p = if p then a else b in
  > let first p = p true in
  > let second p = p false in
  > 
  > let int_pair = pair 1 2 in
  > let int_bool_pair = pair false true in
  > 
  > (first int_pair, second int_bool_pair)
  inferred: int * bool

  $ infer -stru << EOF
  > type 'a list =
  > | Cons of 'a * 'a list
  > | Nil
  > 
  > let empty = Nil
  > 
  > let int_ls = Cons (1, empty)
  > let bool_ls = Cons (true, empty)
  typed structure:
  type 'ty0 list  =
  | Cons of 'ty0 * 'ty0 list
  | Nil
  
  let empty: 'ty2 list = Nil
  
  let int_ls: int list = Cons (1, empty)
  
  let bool_ls: bool list = Cons (true, empty)

  $ infer -stru << EOF
  > type 'a list =
  >   | Cons of 'a * 'a list
  >   | Nil
  > 
  > let is_empty ls =
  >   match ls with
  >   | Nil -> true
  >   | Cons _ -> false
  > 
  > let main = is_empty Nil, is_empty (Cons (1, Nil)), is_empty (Cons (Nil, Nil))
  typed structure:
  type 'ty0 list  =
  | Cons of 'ty0 * 'ty0 list
  | Nil
  
  let is_empty: 'ty4 list -> bool = fun ls -> match ls with | Nil -> true
  | Cons _ -> false
  
  let main: bool * bool * bool = is_empty Nil, is_empty (Cons (1, Nil)), is_empty (Cons (Nil, Nil))
#

# structure
  $ infer -stru << EOF
  > let x = 5
  typed structure:
  let x: int = 5

  $ infer -stru << EOF
  > let (x, y) = (1, true)
  > 
  > let (z, w) = (x, y)
  typed structure:
  let x, y: int * bool = 1, true
  
  let z, w: int * bool = x, y

  $ infer -stru << EOF
  > let apply f x = f x
  > 
  > let y = apply (fun x -> x) 1
  typed structure:
  let apply: ('ty1 -> 'ty2) -> 'ty1 -> 'ty2 = fun f -> fun x -> f x
  
  let y: int = (apply (fun x -> x)) 1

  $ infer -stru << EOF
  > type 'a option =
  > | Some of 'a
  > | None
  > 
  > let x = Some 42
  typed structure:
  type 'ty0 option  =
  | Some of 'ty0
  | None
  
  let x: int option = Some 42

  $ infer -stru << EOF
  > type ('ok, 'err) result =
  > | Ok of 'ok
  > | Error of 'err
  > 
  > let ok = Ok 666
  > let err = Error false
  typed structure:
  type 'ty0, 'ty1 result  =
  | Ok of 'ty0
  | Error of 'ty1
  
  let ok: (int, 'ty4) result = Ok 666
  
  let err: ('ty6, bool) result = Error false

  $ infer -stru << EOF
  > type 'a list =
  > | Cons of 'a * 'a list 
  > | Nil
  > 
  > let main =
  >   match (Cons (1, Nil)) with
  >   | Cons (x, Nil) -> x 
  >   | Nil -> 0
  typed structure:
  type 'ty0 list  =
  | Cons of 'ty0 * 'ty0 list
  | Nil
  
  let main: int = match Cons (1, Nil) with | Cons (x, Nil) -> x
  | Nil -> 0

  $ infer -stru << EOF
  > type ('ok, 'err) result =
  > | Ok of 'ok
  > | Error of 'err
  > 
  > let main =
  >   match (Ok 1) with
  >   | Ok x -> x 
  >   | Error x -> if x then 1 else 0
  typed structure:
  type 'ty0, 'ty1 result  =
  | Ok of 'ty0
  | Error of 'ty1
  
  let main: int = match Ok 1 with | Ok x -> x
  | Error x -> if x then 1 else 0

  $ infer -stru << EOF
  > type 'a option =
  > | Some of 'a
  > | None
  > 
  > let main =
  >   match None with
  >   | None -> 0
  >   | Some x -> x
  typed structure:
  type 'ty0 option  =
  | Some of 'ty0
  | None
  
  let main: int = match None with | None -> 0
  | Some x -> x
#

# should fail
  $ infer -expr << EOF
  > (fun f x -> f x) (fun x -> if x then false else true) 1
  inferencer error: unification of bool and int failed

  $ infer -expr << EOF
  > (fun f g x -> g (f x)) (fun x -> x == 0) (fun x -> if x then 1 else 0) true
  inferencer error: unification of int and bool failed

  $ infer -expr << EOF
  > (fun f x -> f (f x)) (fun x -> x + 1) true
  inferencer error: unification of int and bool failed

  $ infer -expr << EOF
  > (fun (x, y, z) -> x + y < z) (1, 2)
  inferencer error: unification of int * int * int and int * int failed

  $ infer -expr << EOF
  > (fun (p, a, b) -> if p then a else b) (0, 1, 2)
  inferencer error: unification of bool and int failed
#

# unsorted
  $ infer -expr << EOF
  > let apply f x = f x in
  > let not x = if x then false else true in
  > apply not true
  inferred: bool

  $ infer -expr << EOF
  > let twice f x = f (f x) in
  > let not x = if x then false else true in
  > twice not true
  inferred: bool

  $ infer -expr << EOF
  > let compose f g x = g (f x) in
  > let not x = if x then false else true in
  > let is_zero x = x == 0 in
  > compose is_zero not 1
  inferred: bool

  $ infer -expr << EOF
  > let compose f g x = g (f x) in
  > let not x = if x then false else true in
  > let is_zero x = x == 0 in
  > compose is_zero not 1
  inferred: bool

  $ infer -expr << EOF
  > let inc x = x + 1 in
  > inc
  inferred: int -> int

  $ infer -expr << EOF
  > let twice f x = f (f x) in twice
  inferred: ('ty5 -> 'ty5) -> 'ty5 -> 'ty5

  $ infer -expr << EOF
  > let twice f x = f (f x) in
  > let not x = if x then false else true in
  > twice not
  inferred: bool -> bool

  $ infer -expr << EOF
  > let apply f x = f x in
  > let not x = if x then false else true in
  > let inc x = x + 1 in
  > apply not, apply inc
  inferred: (bool -> bool) * (int -> int)


  $ infer -stru << EOF
  > let apply f x = f x
  typed structure:
  let apply: ('ty1 -> 'ty2) -> 'ty1 -> 'ty2 = fun f -> fun x -> f x

  $ infer -stru << EOF
  > let apply f x = f x
  > let inc = apply (fun x -> x + 1)
  typed structure:
  let apply: ('ty1 -> 'ty2) -> 'ty1 -> 'ty2 = fun f -> fun x -> f x
  
  let inc: int -> int = apply (fun x -> x + 1)

  $ infer -stru << EOF
  > let apply f x = f x
  > let not = apply (fun x -> if x then false else true)
  typed structure:
  let apply: ('ty1 -> 'ty2) -> 'ty1 -> 'ty2 = fun f -> fun x -> f x
  
  let not: bool -> bool = apply (fun x -> if x then false else true)

  $ infer -stru << EOF
  > let apply f x = f x
  > and inc = apply (fun x -> x + 1)
  > and not = apply (fun x -> if x then false else true)
  typed structure:
  let apply: ('ty1 -> 'ty2) -> 'ty1 -> 'ty2 = fun f -> fun x -> f x
  and inc: int -> int = apply (fun x -> x + 1)
  and not: bool -> bool = apply (fun x -> if x then false else true)
  $ infer -expr << EOF
  > (fun f x -> f x) (fun x -> if x then false else true) true
  inferred: bool

  $ infer -expr << EOF
  > (fun f g x -> g (f x)) (fun x -> x == 0) (fun x -> if x then 1 else 0) 0
  inferred: int

  $ infer -expr << EOF
  > (fun f x -> f (f x)) (fun x -> x + 1) 0
  inferred: int

  $ infer -expr << EOF
  > (fun (x, y, z) -> x + y < z) (1, 2, 3)
  inferred: bool

  $ infer -expr << EOF
  > (fun (p, a, b) -> if p then a else b) (true, 1, 2)
  inferred: int
#

# should pass 
  $ infer -stru << EOF
  > let rec fac n = if n < 2 then 1 else n * fac (n - 1)
  typed structure:
  let rec fac: int -> int = fun n -> if n < 2 then 1 else n * fac (n - 1)

  $ infer -stru << EOF
  > let rec loop x = loop x
  typed structure:
  let rec loop: 'ty1 -> 'ty2 = fun x -> loop x

  $ infer -stru << EOF
  > let rec apply_n f x n = if n < 1 then x else apply_n f (f x) (n - 1)
  typed structure:
  let rec apply_n: ('ty7 -> 'ty7) -> 'ty7 -> int -> 'ty7 = fun f -> fun x -> fun n -> if n < 1 then x else ((apply_n f) (f x)) (n - 1)

  $ infer -stru << EOF
  > let rec apply_loop f x = apply_loop f (f x)
  typed structure:
  let rec apply_loop: ('ty2 -> 'ty2) -> 'ty2 -> 'ty5 = fun f -> fun x -> (apply_loop f) (f x)

  $ infer -stru << EOF
  > let rec overflow x = 1 + overflow x
  typed structure:
  let rec overflow: 'ty1 -> int = fun x -> 1 + overflow x

  $ infer -stru << EOF
  > let not x = if x then false else true
  > let rec is_even_pos n = if n == 0 then true else not (is_even_pos (n - 1))
  > let rec is_even_neg n = if n == 0 then true else not (is_even_pos (n + 1))
  > let is_even n = if n < 0 then is_even_neg n else is_even_pos n
  typed structure:
  let not: bool -> bool = fun x -> if x then false else true
  
  let rec is_even_pos: int -> bool = fun n -> if n == 0 then true else not (is_even_pos (n - 1))
  
  let rec is_even_neg: int -> bool = fun n -> if n == 0 then true else not (is_even_pos (n + 1))
  
  let is_even: int -> bool = fun n -> if n < 0 then is_even_neg n else is_even_pos n


  $ infer -stru << EOF
  > let abs n = if n < 0 then 0 - n else n
  > let not b = if b then false else true
  > let rec is_even_pos n = if n == 0 then true else not (is_even_pos (n - 1))
  > let is_even n = is_even_pos (abs n)
  typed structure:
  let abs: int -> int = fun n -> if n < 0 then 0 - n else n
  
  let not: bool -> bool = fun b -> if b then false else true
  
  let rec is_even_pos: int -> bool = fun n -> if n == 0 then true else not (is_even_pos (n - 1))
  
  let is_even: int -> bool = fun n -> is_even_pos (abs n)
#

# expr
  $ infer -expr << EOF
  > let rec fac n = if n < 2 then 1 else n * fac (n - 1) in
  > 
  > (fac 1, fac 2 + fac 3)
  inferred: int * int

  $ infer -expr << EOF
  > let abs n = if n < 0 then 0 - n else n in
  > let not b = if b then false else true in
  > let rec is_even_pos n = if n == 0 then true else not (is_even_pos (n - 1)) in
  > let is_even n = is_even_pos (abs n) in
  > 
  > (is_even 1, is_even 2, is_even 3)
  inferred: bool * bool * bool
#

#
  $ infer -stru << EOF
  > let rec fact n = if n < 2 then 1 else n * fact (n - 1)
  typed structure:
  let rec fact: int -> int = fun n -> if n < 2 then 1 else n * fact (n - 1)

  $ infer -stru << EOF
  > let rec fib n = if n < 2 then n else fib (n - 1) + fib (n - 2)
  typed structure:
  let rec fib: int -> int = fun n -> if n < 2 then n else fib (n - 1) + fib (n - 2)
#

# lists
  $ infer -stru << EOF
  > let rec length items =
  >   match items with
  >   | [] -> 0
  >   | _ :: xs -> 1 + length xs
  typed structure:
  let rec length: 'ty3 list -> int = fun items -> match items with | [] -> 0
  | _ :: xs -> 1 + length xs

  $ infer -stru << EOF
  > let rec map f items =
  >   match items with
  >   | [] -> []
  >   | hd :: tl -> f hd :: map f tl 
  typed structure:
  let rec map: ('ty5 -> 'ty9) -> 'ty5 list -> 'ty9 list = fun f ->
    fun items -> match items with | [] -> []
  | hd :: tl -> f hd :: (map f) tl

  $ infer -stru << EOF
  > let rec filter p items = 
  >   match items with
  >   | [] -> []
  >   | hd :: tl -> if p hd then hd :: filter p tl else filter p tl
  typed structure:
  let rec filter: ('ty5 -> bool) -> 'ty5 list -> 'ty5 list = fun p ->
    fun items ->
    match items with | [] -> []
  | hd :: tl -> if p hd then hd :: (filter p) tl else (filter p) tl

  $ infer -stru << EOF
  > let rec filter p items acc = 
  >   match items with
  >   | [] -> acc
  >   | hd :: tl ->
  >     let acc2 = if p hd then hd :: acc else acc in
  >       filter p tl acc2
  > 
  > let filter p items = filter p items []
  typed structure:
  let rec filter: ('ty5 -> bool) -> 'ty5 list -> 'ty5 list -> 'ty5 list = fun p ->
    fun items ->
    fun acc ->
    match items with | [] -> acc
  | hd :: tl -> let acc2 =
    if p hd then hd :: acc else acc in ((filter p) tl) acc2
  
  let filter: ('ty18 -> bool) -> 'ty18 list -> 'ty18 list = fun p -> fun items -> ((filter p) items) []

  $ infer -stru << EOF
  > let rec rev items = 
  >   match items with
  >   | [] -> []
  >   | hd :: tl -> hd :: rev items
  typed structure:
  let rec rev: 'ty4 list -> 'ty4 list = fun items -> match items with | [] -> []
  | hd :: tl -> hd :: rev items

  $ infer -stru << EOF
  > let fold_left f init items =
  >   let rec helper items acc =
  >     match items with
  >       | [] -> acc
  >       | x :: xs -> helper xs (f x acc)
  >   in helper items init
  typed structure:
  let fold_left: ('ty7 -> 'ty15 -> 'ty15) -> 'ty15 -> 'ty7 list -> 'ty15 = fun f ->
    fun init ->
    fun items ->
    let rec helper =
    fun items ->
    fun acc -> match items with | [] -> acc
  | x :: xs -> (helper xs) ((f x) acc) in
    (helper items) init

  $ infer -stru << EOF
  > type 'a list =
  >   | Cons of 'a * 'a list
  >   | Nil
  > 
  > let rec fold f ls acc =
  >   match ls with
  >   | Nil -> acc
  >   | Cons (x, xs) -> fold f xs (f acc x)
  > 
  > let len ls = fold (fun acc _ -> acc + 1) ls 0
  typed structure:
  type 'ty0 list  =
  | Cons of 'ty0 * 'ty0 list
  | Nil
  
  let rec fold: ('ty14 -> 'ty7 -> 'ty14) -> 'ty7 list -> 'ty14 -> 'ty14 = fun f ->
    fun ls ->
    fun acc ->
    match ls with | Nil -> acc
  | Cons (x, xs) -> ((fold f) xs) ((f acc) x)
  
  let len: 'ty17 list -> int = fun ls -> ((fold (fun acc -> fun _ -> acc + 1)) ls) 0

#

  $ infer -stru << EOF
  > type 'a list =
  >   | Cons of 'a * 'a list
  >   | Nil
  > 
  > let rec iter f ls =
  >   match ls with
  >   | Nil -> ()
  >   | Cons (x, xs) ->
  >     let () = f x in
  >       iter f xs
  typed structure:
  type 'ty0 list  =
  | Cons of 'ty0 * 'ty0 list
  | Nil
  
  let rec iter: ('ty6 -> unit) -> 'ty6 list -> unit = fun f ->
    fun ls ->
    match ls with | Nil -> ()
  | Cons (x, xs) -> let () =
    f x in (iter f) xs
