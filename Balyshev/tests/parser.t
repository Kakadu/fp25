  $ INTERPETER="../bin/REPL.exe"

  $ cat << EOF | $INTERPETER -parse -expr 
  > 10 + 20 / 4
  parsed: (10 + (20 / 4))

  $ cat << EOF | $INTERPETER -parse -expr 
  > x * y + (z - w)
  parsed: ((x * y) + (z - w))

  $ cat << EOF | $INTERPETER -parse -expr 
  > (1, (2, 3), (4, 5, 6))
  parsed: (1, (2, 3), (4, 5, 6))

  $ cat << EOF | $INTERPETER -parse -expr 
  > Just (Some None)
  parsed: Just (Some (None))

  $ cat << EOF | $INTERPETER -parse -expr 
  > Just (x + y + z)
  parsed: Just (((x + y) + z))

  $ cat << EOF | $INTERPETER -parse -expr 
  > Just (x, y)
  parsed: Just ((x, y))

  $ cat << EOF | $INTERPETER -parse -expr 
  > let x = 5 in x + 1
  parsed: let x = 5 in (x + 1)

  $ cat << EOF | $INTERPETER -parse -expr 
  > let _ = 5 in 5
  parsed: let _ = 5 in 5

  $ cat << EOF | $INTERPETER -parse -expr 
  > let (x, y, z) = (1, 2, 3) in x + y + z
  parsed: let (x, y, z) = (1, 2, 3) in ((x + y) + z)

  $ cat << EOF | $INTERPETER -parse -expr 
  > let (x, y, z) = (1, 2, 3) in x + y + z
  parsed: let (x, y, z) = (1, 2, 3) in ((x + y) + z)

  $ cat << EOF | $INTERPETER -parse -expr 
  > let _ = Some (Some None) in 1
  parsed: let _ = Some (Some (None)) in 1

  $ cat << EOF | $INTERPETER -parse -expr 
  > let _ = Some (x * y) in x * y
  parsed: let _ = Some ((x * y)) in (x * y)

  $ cat << EOF | $INTERPETER -parse -expr 
  > let _ = Some (x, y) in x, y
  parsed: let _ = Some ((x, y)) in (x, y)

# application
  $ cat << EOF | $INTERPETER -parse -expr 
  > f x
  parsed: f x

  $ cat << EOF | $INTERPETER -parse -expr 
  > f (x, y) (a + b) [q; w; e]
  parsed: f (x, y) (a + b) [ q; w; e ]

  $ cat << EOF | $INTERPETER -parse -expr 
  > f (g x) (h y z)
  parsed: f g x h y z

# lambda

  $ cat << EOF | $INTERPETER -parse -expr 
  > fun _ -> 42
  parsed: (fun _ -> 42)

  $ cat << EOF | $INTERPETER -parse -expr 
  > fun (x, y) -> x
  parsed: (fun (x, y) -> x)

  $ cat << EOF | $INTERPETER -parse -expr 
  > fun x y -> x + y
  parsed: (fun x -> (fun y -> (x + y)))

  $ cat << EOF | $INTERPETER -parse -expr 
  > (fun x -> x, fun x -> x + 1)
  parsed: (fun x -> (x, (fun x -> (x + 1))))

  $ cat << EOF | $INTERPETER -parse -expr 
  > (fun x -> x) (fun x -> x + 1)
  parsed: (fun x -> x) (fun x -> (x + 1))

  $ cat << EOF | $INTERPETER -parse -expr 
  > map (fun x -> x) items
  parsed: map (fun x -> x) items

  $ cat << EOF | $INTERPETER -parse -expr 
  > fun x -> fun y -> y
  parsed: (fun x -> (fun y -> y))

  $ cat << EOF | $INTERPETER -parse -expr 
  > [ fun x -> x; fun x -> x + 1 ]
  parsed: [ (fun x -> x); (fun x -> (x + 1)) ]

  $ cat << EOF | $INTERPETER -parse -expr 
  > map (fun x -> x) (fun y -> y)
  parsed: map (fun x -> x) (fun y -> y)

  $ cat << EOF | $INTERPETER -parse -expr 
  > (fun f g x -> f g x)
  parsed: (fun f -> (fun g -> (fun x -> f g x)))

  $ cat << EOF | $INTERPETER -parse -expr 
  > (fun f g x -> f (g x))
  parsed: (fun f -> (fun g -> (fun x -> f g x)))


# if then else

  $ cat << EOF | $INTERPETER -parse -expr 
  > if 1 then 1 else 0
  parsed: if 1 then 1 else 0

  $ cat << EOF | $INTERPETER -parse -expr 
  > if (a < b) then (a + b) else (a - b)
  parsed: if (a < b) then (a + b) else (a - b)

  $ cat << EOF | $INTERPETER -parse -expr 
  > if (f x) then (fun f -> f x) else (fun x -> f x)
  parsed: if f x then (fun f -> f x) else (fun x -> f x)

  $ cat << EOF | $INTERPETER -parse -expr 
  > fun x -> if x then (fun x -> x) else (fun y -> y)
  parsed: (fun x -> if x then (fun x -> x) else (fun y -> y))

  $ cat << EOF | $INTERPETER -parse -expr 
  > let rec fact = fun n -> if n < 2 then 1 else n * fact (n - 1) in fact 5
  parsed: let rec fact = (fun n -> if (n < 2) then 1 else (n * fact (n - 1))) in fact 5

# match
  $ cat << EOF | $INTERPETER -parse -expr 
  > match (x, y) with
  > | (x, y) -> (x, y)
  > | _ -> (y, x)
  parsed: (match (x, y) with
  | (x, y) -> (x, y)
  | _ -> (y, x))

  $ cat << EOF | $INTERPETER -parse -expr 
  > match e with
  > | (f, (f, (f, (f, s)))) -> 4
  > | (f, (f, (f, s))) -> 3
  > | (f, (f, s)) -> 2
  > | (f, s) -> 1
  > | s -> 0
  parsed: (match e with
  | (f, (f, (f, (f, s)))) -> 4
  | (f, (f, (f, s))) -> 3
  | (f, (f, s)) -> 2
  | (f, s) -> 1
  | s -> 0)
  $ cat << EOF | $INTERPETER -parse -expr 
  > match x with
  > | One x -> 1
  > | Two (x, y) -> 2
  > | Three (x, y, z) -> 3
  parsed: (match x with
  | One (x) -> 1
  | Two ((x, y)) -> 2
  | Three ((x, y, z)) -> 3)

  $ cat << EOF | $INTERPETER -parse -expr 
  > match x with
  > | _ -> if x then y else z
  parsed: (match x with
  | _ -> if x then y else z)

  $ cat << EOF | $INTERPETER -parse -expr 
  > if x then 
  >   match y with
  >   | _ -> y
  > else 
  >   match z with
  >   | _ -> Z
  parsed: if x then (match y with
  | _ -> y) else (match z with
  | _ -> Z)

  $ cat << EOF | $INTERPETER -parse -expr 
  > match f x with
  > | Some x -> g x
  > | None -> a b c
  parsed: (match f x with
  | Some (x) -> g x
  | None -> a b c)

  $ cat << EOF | $INTERPETER -parse -expr 
  > match x with
  > | A -> a
  > | B ->
  >   match y with
  >   | C -> c
  >   | D -> d
  parsed: (match x with
  | A -> a
  | B -> (match y with
  | C -> c
  | D -> d))

  $ cat << EOF | $INTERPETER -parse -expr 
  > match x with
  > | A -> 
  >   (match y with
  >    | C -> c
  >    | D -> d)
  > | B -> b
  parsed: (match x with
  | A -> (match y with
  | C -> c
  | D -> d)
  | B -> b)

  $ cat << EOF | $INTERPETER -parse -expr 
  > match (match () with _ -> ()) with
  > | _ -> ()
  parsed: (match (match () with
  | _ -> ()) with
  | _ -> ())

  $ cat << EOF | $INTERPETER -parse -expr 
  > match (if x then y else z) with
  > | _ -> ()
  parsed: (match if x then y else z with
  | _ -> ())

  $ cat << EOF | $INTERPETER -parse -expr 
  > if (match x with _ -> ()) then 1 else 2
  parsed: if (match x with
  | _ -> ()) then 1 else 2
#

# value binding chains
  $ cat << EOF | $INTERPETER -parse -expr 
  > let x = 1 and y = 2 in x + y
  parsed: let x = 1 and y = 2 in (x + y)

  $ cat << EOF | $INTERPETER -parse -expr 
  > let (x, y) = (1, 2) and (z, w) = (3, 4) in x, y, z, w
  parsed: let (x, y) = (1, 2) and (z, w) = (3, 4) in (x, y, z, w)

  $ cat << EOF | $INTERPETER -parse -expr 
  > let _ = 1 and x = 2 and (a, b) = 3 in 0
  parsed: let _ = 1 and x = 2 and (a, b) = 3 in 0

  $ cat << EOF | $INTERPETER -parse -expr 
  > let Some x = 1 and [ x; y ] = 2 and z :: w = 3 in 0
  parsed: let Some (x) = 1 and [ x; y ] = 2 and z :: w = 3 in 0
#

# core_type
  $ cat << EOF | $INTERPETER -parse -core-type
  > int
  parsed: int

  $ cat << EOF | $INTERPETER -parse -core-type
  > (int)
  parsed: int

  $ cat << EOF | $INTERPETER -parse -core-type
  > 'a
  parsed: 'a

  $ cat << EOF | $INTERPETER -parse -core-type
  > ('a)
  parsed: 'a

  $ cat << EOF | $INTERPETER -parse -core-type
  > 'a * 'b
  parsed: ('a * 'b)

  $ cat << EOF | $INTERPETER -parse -core-type
  > 'a -> 'b
  parsed: ('a -> 'b)

  $ cat << EOF | $INTERPETER -parse -core-type
  > ('a -> 'b) * ('b -> 'a)
  parsed: (('a -> 'b) * ('b -> 'a))

  $ cat << EOF | $INTERPETER -parse -core-type
  > int list
  parsed: (int) list

  $ cat << EOF | $INTERPETER -parse -core-type
  > ('a) list
  parsed: ('a) list

  $ cat << EOF | $INTERPETER -parse -core-type
  > ('a * 'b) list
  parsed: (('a * 'b)) list

  $ cat << EOF | $INTERPETER -parse -core-type
  > ('a -> 'b -> 'c) list
  parsed: (('a -> ('b -> 'c))) list

  $ cat << EOF | $INTERPETER -parse -core-type
  > ('a, 'b) list
  parsed: ('a, 'b) list

  $ cat << EOF | $INTERPETER -parse -core-type
  > ('a -> 'b, 'c * 'd) list
  parsed: (('a -> 'b), ('c * 'd)) list
#

# type declaration
  $ cat << EOF | $INTERPETER -parse -stru
  > type t = int
  parsed: type t = int
  
  $ cat << EOF | $INTERPETER -parse -stru
  > type 'a my_list = 'a list
  parsed: type 'a my_list = ('a) list
  

  $ cat << EOF | $INTERPETER -parse -stru
  > type ('a, 'b) pair = 'a * 'b
  parsed: type ('a, 'b) pair = ('a * 'b)
  

  $ cat << EOF | $INTERPETER -parse -stru
  > type ('a, 'b) arrow = 'a -> 'b
  parsed: type ('a, 'b) arrow = ('a -> 'b)
  
#

# something more complex
  $ cat << EOF | $INTERPETER -parse -stru
  > type 'a option =
  > | Some of 'a
  > | None
  parsed: type 'a option = | Some of 'a
  | None
  
  

  $ cat << EOF | $INTERPETER -parse -stru
  > type 'a list =
  > | Nil
  > | Cons of 'a * 'a list
  parsed: type 'a list = | Nil
  | Cons of ('a * ('a) list)
  
  

  $ cat << EOF | $INTERPETER -parse -stru
  > type ('a, 'b) qwe =
  > | Asd of 'a -> ('a -> 'b) -> 'b
  > | Zxc of ('a -> 'a) * ('a -> 'a) * 'a
  parsed: type ('a, 'b) qwe =
  | Asd of ('a -> (('a -> 'b) -> 'b))
  | Zxc of (('a -> 'a) * ('a -> 'a) * 'a)
  
  
#

# "and" chains
  $ cat << EOF | $INTERPETER -parse -stru
  > type a = int
  > and b = bool
  > and c = char
  parsed: type a = int
  and b = bool
  and c = char
  

  $ cat << EOF | $INTERPETER -parse -stru
  > type 'a box =
  > | Box of 'a
  > 
  > and 't maybe =
  > | Just of 't
  > | Nothing
  > 
  > and name = string
  parsed: type 'a box = | Box of 'a
  
  and 't maybe = | Just of 't
  | Nothing
  
  and name = string
  

# unsorted
  $ cat << EOF | $INTERPETER -parse -stru
  > type foo = 'a * 'b * 'c -> 'd * 'e -> 'f
  parsed: type foo = (('a * 'b * 'c) -> (('d * 'e) -> 'f))
  

  $ cat << EOF | $INTERPETER -parse -stru
  > type foo = (int -> int)
  parsed: type foo = (int -> int)
  

  $ cat << EOF | $INTERPETER -parse -stru
  > type foo = (a -> b) * (c -> d)
  parsed: type foo = ((a -> b) * (c -> d))
  

  $ cat << EOF | $INTERPETER -parse -stru
  > type foo = (a * b) * (c * d)
  parsed: type foo = ((a * b) * (c * d))
  

  $ cat << EOF | $INTERPETER -parse -stru
  > type foo = (a -> b) -> (c -> d)
  parsed: type foo = ((a -> b) -> (c -> d))
  
#
