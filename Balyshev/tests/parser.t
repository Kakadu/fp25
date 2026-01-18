  $ parse () { ../bin/REPL.exe -parse "$@"; }

  $ parse -expr << EOF
  > 1 + 2 * 3 / 4 - 5
  parsed:
  (1 + ((2 * 3) / 4)) - 5

  $ parse -expr << EOF
  > a b c
  parsed:
  (a b) c

  $ parse -expr << EOF
  > x * y + (z - w)
  parsed:
  (x * y) + (z - w)

  $ parse -expr << EOF
  > (1, (2, 3), (4, 5, 6))
  parsed:
  1, (2, 3), (4, 5, 6)

  $ parse -expr << EOF
  > Just (Some None)
  parsed:
  Just (Some None)

  $ parse -expr << EOF
  > Just (x + y + z)
  parsed:
  Just ((x + y) + z)

  $ parse -expr << EOF
  > Just (x, y, z, w)
  parsed:
  Just (x, y, z, w)

  $ parse -expr << EOF
  > Just [ x; y; z; w ]
  parsed:
  Just [ x; y; z; w ]

  $ parse -expr << EOF
  > let x = 5 in x + 1
  parsed:
  let x =
    5 in x + 1

  $ parse -expr << EOF
  > let _ = 5 in 5
  parsed:
  let _ =
    5 in 5

  $ parse -expr << EOF
  > let (x, y, z) = (1, 2, 3) in x + y + z
  parsed:
  let (x, y, z) =
    1, 2, 3 in (x + y) + z

  $ parse -expr << EOF
  > let (x, y, z) = (1, 2, 3) in x + y + z
  parsed:
  let (x, y, z) =
    1, 2, 3 in (x + y) + z

  $ parse -expr << EOF
  > let _ = Some (Some None) in 1
  parsed:
  let _ =
    Some (Some None) in 1

  $ parse -expr << EOF
  > let _ = Some (x * y) in x * y
  parsed:
  let _ =
    Some (x * y) in x * y

  $ parse -expr << EOF
  > let _ = Some (x, y) in x, y
  parsed:
  let _ =
    Some (x, y) in x, y

# application
  $ parse -expr << EOF
  > f x
  parsed:
  f x

  $ parse -expr << EOF
  > f (g x) (h y z)
  parsed:
  (f (g x)) ((h y) z)
#

# patterns
  $ parse -patt  << EOF
  > (x, y), _
  parsed: (x, y), _

  $ parse -patt  << EOF
  > Some (x, y, _)
  parsed: Some (x, y, _)

  $ parse -patt  << EOF
  > Just x, Some (x, y), Box _
  parsed: Just x, Some (x, y), Box _

  $ parse -patt  << EOF
  > Just (Some None)
  parsed: Just (Some None)

  $ parse -patt  << EOF
  > x :: y :: z :: w
  parsed: x :: y :: z :: w

  $ parse -patt  << EOF
  > x :: y :: z :: w :: []
  parsed: [ x; y; z; w ]

  $ parse -patt  << EOF
  > Some x :: Some []
  parsed: Some x :: Some []

  $ parse -patt  << EOF
  > Some (x :: y)
  parsed: Some (x :: y)

  $ parse -patt  << EOF
  > (x, y :: z)
  parsed: x, y :: z

  $ parse -patt  << EOF
  > [ x; y; z ] :: []
  parsed: [ [ x; y; z ] ]

  $ parse -patt  << EOF
  > Some [ x; y ]
  parsed: Some [ x; y ]

  $ parse -patt  << EOF
  > [ (x, y); (a, b) ]
  parsed: [ x, y; a, b ]

  $ parse -patt  << EOF
  > ( [ x; y ], [ a; b ] )
  parsed: [ x; y ], [ a; b ]
#

# lambda
  $ parse -expr << EOF
  > fun _ -> 42
  parsed:
  fun _ -> 42

  $ parse -expr << EOF
  > fun (x, y) -> x
  parsed:
  fun (x, y) -> x

  $ parse -expr << EOF
  > fun x y -> x + y
  parsed:
  fun x -> fun y -> x + y

  $ parse -expr << EOF
  > (fun x -> x, fun x -> x + 1)
  parsed:
  fun x -> x, (fun x -> x + 1)

  $ parse -expr << EOF
  > (fun x -> x) (fun x -> x + 1)
  parsed:
  (fun x -> x) (fun x -> x + 1)

  $ parse -expr << EOF
  > map (fun x -> x) items
  parsed:
  (map (fun x -> x)) items

  $ parse -expr << EOF
  > fun x -> fun y -> y
  parsed:
  fun x -> fun y -> y

  $ parse -expr << EOF
  > map (fun x -> x) (fun y -> y)
  parsed:
  (map (fun x -> x)) (fun y -> y)

  $ parse -expr << EOF
  > (fun f g x -> f g x)
  parsed:
  fun f -> fun g -> fun x -> (f g) x

  $ parse -expr << EOF
  > (fun f g x -> f (g x))
  parsed:
  fun f -> fun g -> fun x -> f (g x)


# if then else
  $ parse -expr << EOF
  > if 1 then 1 else 0
  parsed:
  if 1 then 1 else 0

  $ parse -expr << EOF
  > if (a < b) then (a + b) else (a - b)
  parsed:
  if a < b then a + b else a - b

  $ parse -expr << EOF
  > if (f x) then (fun f -> f x) else (fun x -> f x)
  parsed:
  if f x then fun f -> f x else fun x -> f x

  $ parse -expr << EOF
  > fun x -> if x then (fun x -> x) else (fun y -> y)
  parsed:
  fun x -> if x then fun x -> x else fun y -> y

  $ parse -expr << EOF
  > let rec fact = fun n -> if n < 2 then 1 else n * fact (n - 1) in fact 5
  parsed:
  let rec fact =
    fun n -> if n < 2 then 1 else n * fact (n - 1) in fact 5

# match
  $ parse -expr << EOF
  > match (x, y) with
  > | (x, y) -> (x, y)
  > | _ -> (y, x)
  parsed:
  match x, y with | x, y -> x, y
  | _ -> y, x

  $ parse -expr << EOF
  > match e with
  > | (f, (f, (f, (f, s)))) -> 4
  > | (f, (f, (f, s))) -> 3
  > | (f, (f, s)) -> 2
  > | (f, s) -> 1
  > | s -> 0
  parsed:
  match e with | f, (f, (f, (f, s))) -> 4 | f, (f, (f, s)) -> 3
  | f, (f, s) -> 2 | f, s -> 1
  | s -> 0
  $ parse -expr << EOF
  > match x with
  > | One x -> 1
  > | Two (x, y) -> 2
  > | Three (x, y, z) -> 3
  parsed:
  match x with | One x -> 1 | Two (x, y) -> 2
  | Three (x, y, z) -> 3

  $ parse -expr << EOF
  > match x with
  > | _ -> if x then y else z
  parsed:
  match x with
  | _ -> if x then y else z

  $ parse -expr << EOF
  > if x then 
  >   match y with
  >   | _ -> y
  > else 
  >   match z with
  >   | _ -> Z
  parsed:
  if x then match y with
  | _ -> y else match z with
  | _ -> Z

  $ parse -expr << EOF
  > match f x with
  > | Some x -> g x
  > | None -> a b c
  parsed:
  match f x with | Some x -> g x
  | None -> (a b) c

  $ parse -expr << EOF
  > match x with
  > | A -> a
  > | B ->
  >   match y with
  >   | C -> c
  >   | D -> d
  parsed:
  match x with | A -> a
  | B -> (match y with | C -> c
  | D -> d)

  $ parse -expr << EOF
  > match x with
  > | A -> 
  >   (match y with
  >    | C -> c
  >    | D -> d)
  > | B -> b
  parsed:
  match x with | A -> (match y with | C -> c
  | D -> d)
  | B -> b

  $ parse -expr << EOF
  > match (match () with _ -> ()) with
  > | _ -> ()
  parsed:
  match (match () with
  | _ -> ()) with
  | _ -> ()

  $ parse -expr << EOF
  > match (if x then y else z) with
  > | _ -> ()
  parsed:
  match if x then y else z with
  | _ -> ()

  $ parse -expr << EOF
  > if (match x with _ -> ()) then 1 else 2
  parsed:
  if match x with
  | _ -> () then 1 else 2
#

# value binding chains
  $ parse -expr << EOF
  > let x = 1 and y = 2 in x + y
  parsed:
  let x =
    1
  and y =
    2 in x + y

  $ parse -expr << EOF
  > let (x, y) = (1, 2) and (z, w) = (3, 4) in x, y, z, w
  parsed:
  let (x, y) =
    1, 2
  and (z, w) =
    3, 4 in x, y, z, w

  $ parse -expr << EOF
  > let _ = 1 and x = 2 and (a, b) = 3 in 0
  parsed:
  let _ =
    1 and x =
    2
  and (a, b) =
    3 in 0
#

# core_type
  $ parse -core-type << EOF
  > int
  parsed: int

  $ parse -core-type << EOF
  > (int)
  parsed: int

  $ parse -core-type << EOF
  > 'a
  parsed: 'a

  $ parse -core-type << EOF
  > ('a)
  parsed: 'a

  $ parse -core-type << EOF
  > 'a * 'b
  parsed: 'a * 'b

  $ parse -core-type << EOF
  > 'a -> 'b
  parsed: 'a -> 'b

  $ parse -core-type << EOF
  > ('a -> 'b) * ('b -> 'a)
  parsed: ('a -> 'b) * ('b -> 'a)

  $ parse -core-type << EOF
  > int list
  parsed: int list

  $ parse -core-type << EOF
  > ('a) list
  parsed: 'a list

  $ parse -core-type << EOF
  > ('a * 'b) list
  parsed: ('a * 'b) list

  $ parse -core-type << EOF
  > ('a -> 'b -> 'c) list
  parsed: ('a -> 'b -> 'c) list

  $ parse -core-type << EOF
  > ('a, 'b) list
  parsed: ('a, 'b) list

  $ parse -core-type << EOF
  > ('a -> 'b, 'c * 'd) list
  parsed: ('a -> 'b, 'c * 'd) list
#

# type declarations
  $ parse -stru << EOF
  > type t = int
  parsed:
  type t = int
  
  $ parse -stru << EOF
  > type 'a my_list = 'a list
  parsed:
  type 'a my_list = 'a list
  

  $ parse -stru << EOF
  > type ('a, 'b) pair = 'a * 'b
  parsed:
  type ('a, 'b) pair = 'a * 'b
  

  $ parse -stru << EOF
  > type ('a, 'b) arrow = 'a -> 'b
  parsed:
  type ('a, 'b) arrow = 'a -> 'b
  

  $ parse -stru << EOF
  > type 'a option =
  > | Some of 'a
  > | None
  parsed:
  type 'a option = | Some of 'a
  | None
  
  

  $ parse -stru << EOF
  > type 'a list =
  > | Nil
  > | Cons of 'a * 'a list
  parsed:
  type 'a list = | Nil
  | Cons of 'a * 'a list
  
  

  $ parse -stru << EOF
  > type ('a, 'b) qwe =
  > | Asd of 'a -> ('a -> 'b) -> 'b
  > | Zxc of ('a -> 'a) * ('a -> 'a) * 'a
  parsed:
  type ('a, 'b) qwe = | Asd of 'a -> ('a -> 'b) -> 'b
  | Zxc of ('a -> 'a) * ('a -> 'a) * 'a
  
  
#

# "and" chains
  $ parse -stru << EOF
  > type a = int
  > and b = bool
  > and c = char
  parsed:
  type a = int
  and b = bool
  and c = char
  

  $ parse -stru << EOF
  > type 'a box =
  > | Box of 'a
  > 
  > and 't maybe =
  > | Just of 't
  > | Nothing
  > 
  > and name = string
  parsed:
  type 'a box = | Box of 'a
  
  and 't maybe = | Just of 't
  | Nothing
  
  and name = string
  

# stru
  $ parse -stru << EOF
  > type foo = 'a * 'b * 'c -> 'd * 'e -> 'f
  parsed:
  type foo = 'a * 'b * 'c -> 'd * 'e -> 'f
  

  $ parse -stru << EOF
  > type foo = (int -> int)
  parsed:
  type foo = int -> int
  

  $ parse -stru << EOF
  > type foo = (a -> b) * (c -> d)
  parsed:
  type foo = (a -> b) * (c -> d)
  

  $ parse -stru << EOF
  > type foo = (a * b) * (c * d)
  parsed:
  type foo = (a * b) * (c * d)
  

  $ parse -stru << EOF
  > type foo = (a -> b) -> (c -> d)
  parsed:
  type foo = (a -> b) -> c -> d
  

  $ parse -stru << EOF
  > let rec fact n = if n < 2 then 1 else n * fact (n - 1)
  parsed:
  let rec fact =
    fun n -> if n < 2 then 1 else n * fact (n - 1)

  $ parse -stru << EOF
  > let rec fib n = if n < 2 then 1 else fib (n - 1) + fib (n - 2)
  parsed:
  let rec fib =
    fun n -> if n < 2 then 1 else fib (n - 1) + fib (n - 2)
#

# lists
  $ parse -stru << EOF
  > let rec length items =
  >   match items with
  >   | [] -> 0
  >   | _ :: xs -> 1 + length xs
  parsed:
  let rec length =
    fun items -> match items with | [] -> 0
  | _ :: xs -> 1 + length xs

  $ parse -stru << EOF
  > let rec map f items =
  >   match items with
  >   | [] -> []
  >   | hd :: tl -> f hd :: map f tl 
  parsed:
  let rec map =
    fun f ->
    fun items -> match items with | [] -> []
  | hd :: tl -> f hd :: (map f) tl

  $ parse -stru << EOF
  > let rec filter p items = 
  >   match items with
  >   | [] -> []
  >   | hd :: tl -> if p hd then hd :: filter p tl else filter p tl
  parsed:
  let rec filter =
    fun p ->
    fun items ->
    match items with | [] -> []
  | hd :: tl -> if p hd then hd :: (filter p) tl else (filter p) tl

  $ parse -stru << EOF
  > let rec filter p items acc = 
  >   match items with
  >   | [] -> acc
  >   | hd :: tl ->
  >     let acc2 = if p hd then hd :: acc else acc in
  >       filter p tl acc2
  > 
  > let filter p items = filter p items []
  parsed:
  let rec filter =
    fun p ->
    fun items ->
    fun acc ->
    match items with | [] -> acc
  | hd :: tl -> let acc2 =
    if p hd then hd :: acc else acc in ((filter p) tl) acc2let filter =
    fun p -> fun items -> ((filter p) items) []

  $ parse -stru << EOF
  > let rec rev items = 
  >   match items with
  >   | [] -> []
  >   | hd :: tl -> hd :: rev items
  parsed:
  let rec rev =
    fun items -> match items with | [] -> []
  | hd :: tl -> hd :: rev items

  $ parse -stru << EOF
  > let fold_left f init items =
  >   let rec helper items acc =
  >     match items with
  >       | [] -> acc
  >       | x :: xs -> helper xs (f x acc)
  >   in helper items init
  parsed:
  let fold_left =
    fun f ->
    fun init ->
    fun items ->
    let rec helper =
    fun items ->
    fun acc -> match items with | [] -> acc
  | x :: xs -> (helper xs) ((f x) acc) in
    (helper items) init
#

# nested let
  $ parse -expr << EOF
  > let main = (let x = 1 in x) in main
  parsed:
  let main =
    let x =
    1 in x in main

  $ parse -expr << EOF
  > let main = let x = 1 in x in main
  parsed:
  let main =
    let x =
    1 in x in main

  $ parse -expr << EOF
  > fun x -> let y = 1 in x + y
  parsed:
  fun x -> let y =
    1 in x + y

  $ parse -expr << EOF
  > let x = 1 in x, 1
  parsed:
  let x =
    1 in x, 1

  $ parse -expr << EOF
  > (let x = 1 in x), 1
  parsed:
  (let x =
    1 in x), 1

  $ parse -expr << EOF
  > [ (let x = 1 in x); (let y = 2 in y) ]
  parsed:
  [ let x =
    1 in x; let y =
    2 in y ]

  $ parse -expr << EOF
  > [ let x = 1 in x; let y = 2 in y ]
  parsed:
  [ let x =
    1 in x; let y =
    2 in y ]

  $ parse -expr << EOF
  > if (let x = true in x) then (let y = 1 in y) else (let z = 2 in z)
  parsed:
  if let x =
    true in x then let y =
    1 in y else let z =
    2 in z

  $ parse -expr << EOF
  > if let x = true in x then let y = 1 in y else let z = 2 in z
  parsed:
  if let x =
    true in x then let y =
    1 in y else let z =
    2 in z

  $ parse -expr << EOF
  >   match (let x = 1 in x) with
  >   | 1 -> (let y = 2 in y)
  >   | _ -> (let z = 3 in z)
  parsed:
  match let x =
    1 in x with | 1 -> let y =
    2 in y
  | _ -> let z =
    3 in z

  $ parse -expr << EOF
  >   match let x = 1 in x with
  >   | 1 -> let y = 2 in y
  >   | _ -> let z = 3 in z
  parsed:
  match let x =
    1 in x with | 1 -> let y =
    2 in y
  | _ -> let z =
    3 in z
#


# nested match
  $ parse -expr << EOF
  > let main = (match () with | () -> 0) in main
  parsed:
  let main =
    match () with
  | () -> 0 in main

  $ parse -expr << EOF
  > let main = match () with | () -> 0 in main
  parsed:
  let main =
    match () with
  | () -> 0 in main

  $ parse -expr << EOF
  > fun x ->
  >   (match x with
  >   | () -> 0)
  parsed:
  fun x -> match x with
  | () -> 0

  $ parse -expr << EOF
  > fun x ->
  >   match x with
  >   | () -> 0
  parsed:
  fun x -> match x with
  | () -> 0

  $ parse -expr << EOF
  > (match () with | () -> 0), 0
  parsed:
  (match () with
  | () -> 0), 0

  $ parse -expr << EOF
  > match () with | () -> 0, 0
  parsed:
  match () with
  | () -> 0, 0

  $ parse -expr << EOF
  > [ (match () with | () -> 1); (match () with | () -> 2) ]
  parsed:
  [ match () with
  | () -> 1; match () with
  | () -> 2 ]

  $ parse -expr << EOF
  > [ match () with | () -> 1; match () with | () -> 2 ]
  parsed:
  [ match () with
  | () -> 1; match () with
  | () -> 2 ]

  $ parse -expr << EOF
  > if (match () with | () -> true) then 1 else 2
  parsed:
  if match () with
  | () -> true then 1 else 2

  $ parse -expr << EOF
  > if match () with | () -> true then 1 in 2
  parsing error: : char '['

  $ parse -expr << EOF
  >  match (match 1 with
  >         | 2 -> 3
  >         | 4 -> 5) with
  >  | 6 -> 
  >     (match 7 with
  >      | 8 -> 9
  >      | 10 -> 11)
  >  | 12 -> 
  >      (match 13 with
  >      | 14 -> 15
  >      | 16 -> 17)
  parsed:
  match (match 1 with | 2 -> 3
  | 4 -> 5) with
  | 6 -> (match 7 with | 8 -> 9
  | 10 -> 11)
  | 12 -> (match 13 with | 14 -> 15
  | 16 -> 17)
#

# slow af
$ parse -expr << EOF
> (fun c -> None / 31), (fun (Just (Box _)) ->
> (fun ((a, (Box (Nil, y), Nothing, (y, _, (y, _, _))), Nothing), (Some (Box (x, Nil, b)), (_, Box z))) ->
> (None, (fun y -> a), None > ()), z > (b <= Nil)), (c, ((fun 94 -> ()), (fun (Some true) -> None)) * (fun (Box (Some (Some Nil))) ->
> fun (Just (Box (y, x, (_, Some (Just (Some y)))), Box (Some ((Box c, Some 28, (Nil, y)), (Some Nothing, z), ((36, Nothing, _), (x, false, y))), x, (Box (Box z), Box _, Nothing)), y)) ->
> z))), Nothing
parsed: (fun c -> None / 31), (fun (Just (Box _)) ->
(fun ((a, (Box (Nil, y), Nothing, (y, _, (y, _, _))), Nothing), (Some (Box (x, Nil, b)), (_, Box z))) ->
(None, (fun y -> a), None > ()), z > (b <= Nil)), (c, ((fun 94 -> ()), (fun (Some true) -> None)) * (fun (Box (Some (Some Nil))) ->
fun (Just (Box (y, x, (_, Some (Just (Some y)))), Box (Some ((Box c, Some 28, (Nil, y)), (Some Nothing, z), ((36, Nothing, _), (x, false, y))), x, (Box (Box z), Box _, Nothing)), y)) ->
z))), Nothing

$ parse -expr << EOF
>  fun (Some (Just (Just Nothing)), (Just (Some Nil, Box (_, Some false, _)), None, (((Some (), _, Some (Nil, _)), Some Nil), (_, (Nil, (Box (), (_, Nothing)))))), (((Just _, ((Box (), z, None), Nothing), (Just _, (c, Box ()))), Box a), (true, (true, Just (), Box (Box (Some _))), Box Nil), Box ((Just (_, false), 23), Just ()))) -> 0
parsed: fun (Some (Just (Just Nothing)), (Just (Some Nil, Box (_, Some false, _)), None, (((Some (), _, Some (Nil, _)), Some Nil), (_, (Nil, (Box (), (_, Nothing)))))), (((Just _, ((Box (), z, None), Nothing), (Just _, (c, Box ()))), Box a), (true, (true, Just (), Box (Box (Some _))), Box Nil), Box ((Just (_, false), 23), Just ()))) ->
0

$ parse -expr << EOF
> (Just (Just (Just c))) ((if (Just (Nil z)) (Some z <> z) then Box ((None == c) (Box Nil)) else Box (false == ()) < ((if c then Nil else false) >= Box a)) (Just ((if 70 > None then () else y > ()) (Box (if () then z else true)))))
parsed: (Just (Just (Just c))) ((if (Just (Nil z)) (Some z <> z) then Box ((None == c) (Box Nil)) else Box (false == ()) < ((if c then Nil else false) >= Box a)) (Just ((if 70 > None then () else y > ()) (Box (if () then z else true)))))

$ parse -expr << EOF
> if (if (Nothing - Nil) / (if z then Nil else None) then 54 else y) then 55 ((if Nothing then 1 else z) (c <= ())) else (if (if true b then None Nil else Nil * Nothing) then (if true c then () else 94 <= 5) else (if 62 then Nil else Nothing) <> (Nil > Nothing))
parsed: if (if (Nothing - Nil) / (if z then Nil else None) then 54 else y) then 55 ((if Nothing then 1 else z) (c <= ())) else (if (if true b then None Nil else Nil * Nothing) then (if true c then () else 94 <= 5) else (if 62 then Nil else Nothing) <> (Nil > Nothing))

$ parse -expr << EOF
> ((((fun ([ _ ], [ b; 70 ]) -> fun [ _ ] -> None), [ None; 19 ], (fun (Some (Some _, (Nil, _, a))) ->
>   fun [ Just (Just []); Box (Some ([ _; _; _ ], (_, Nil, z)), [ Some (Just (c, _, z)) ]) ] ->
>   Nil)), []), ([], (fun [ a, x, Nil ] ->
>   fun (Some (Some (Just [ Just Nothing; [ c; _; a ]; [ Nil; x ] ]))) ->
>   (Nothing <= Nil) <> (fun ([ Box Nothing, [ _; c; () ] ], [ Just (Just (Nil, Nil, Nil)) ], (Some [ None; y ], [])) ->
>   false)), ((b, a), [ None; Nil ], []) + ([] <= (a, x, ()))), ([], ((fun ([ [ Box (43, ()) ]; [ (y, z), (x, z), Just true; Box (_, _, _) ]; [] ], [], (((Just Nothing, (40, x, true), (Nil, 20)), Some (Some (c, Nil)), [ 68, None; None, 22, _ ]), [ [ Nothing, 65, Nil; Some None ] ], [ [ Box Nothing ]; [ Box _; [ None ]; Just z ]; Some y, (_, b, 42), (x, a, ()) ])) ->
>   []), (fun (None, _, c) -> fun (Box (Nil, b)) -> 63), []))) <> [ (((93 * true) >= (Nil, (), y)) < (fun [] -> () < c), ((95, z), (true, y, None), (b, false, x)), ((a / b) - (31 <= x)) >= [ z; c; 39 ]) + ((fun [ _; None ] -> c, z, x) - [], (y + x, (None, y, y), (fun (Box ([ None, _; z, _, Nothing; [ 91; Nil ] ], [], Just (Just []))) ->
>   None)), (40, true) < (fun [ Box (Some (Box (Just None)), ([ a; _ ], Just z)), [ Some [ [ _ ] ]; [ Some (b, z); Box (Some y) ]; Box (Just [ _ ]) ]; [ [ ((), _, _), (b, a); (c, Nil), Just (Some _); Some (Box Nothing, Some 88) ] ]; Some (Some [ Just (Just y) ]), Some ([], (Box _, Just Nothing), Some (_, y, false)), (([ Nil; _ ], [], Just [ None; y; false ]), Just ([ 5; y; _ ], Just None, [ _ ]), ([], Box (Some z), (true, z, _))) ] ->
>   z, b)) ]
parsed: ((((fun ([ _ ], [ b; 70 ]) -> fun [ _ ] -> None), [ None; 19 ], (fun (Some (Some _, (Nil, _, a))) ->
fun [ Just (Just []); Box (Some ([ _; _; _ ], (_, Nil, z)), [ Some (Just (c, _, z)) ]) ] ->
Nil)), []), ([], (fun [ a, x, Nil ] ->
fun (Some (Some (Just [ Just Nothing; [ c; _; a ]; [ Nil; x ] ]))) ->
(Nothing <= Nil) <> (fun ([ Box Nothing, [ _; c; () ] ], [ Just (Just (Nil, Nil, Nil)) ], (Some [ None; y ], [])) ->
false)), ((b, a), [ None; Nil ], []) + ([] <= (a, x, ()))), ([], ((fun ([ [ Box (43, ()) ]; [ (y, z), (x, z), Just true; Box (_, _, _) ]; [] ], [], (((Just Nothing, (40, x, true), (Nil, 20)), Some (Some (c, Nil)), [ 68, None; None, 22, _ ]), [ [ Nothing, 65, Nil; Some None ] ], [ [ Box Nothing ]; [ Box _; [ None ]; Just z ]; Some y, (_, b, 42), (x, a, ()) ])) ->
[]), (fun (None, _, c) -> fun (Box (Nil, b)) -> 63), []))) <> [ (((93 * true) >= (Nil, (), y)) < (fun [] -> () < c), ((95, z), (true, y, None), (b, false, x)), ((a / b) - (31 <= x)) >= [ z; c; 39 ]) + ((fun [ _; None ] -> c, z, x) - [], (y + x, (None, y, y), (fun (Box ([ None, _; z, _, Nothing; [ 91; Nil ] ], [], Just (Just []))) ->
None)), (40, true) < (fun [ Box (Some (Box (Just None)), ([ a; _ ], Just z)), [ Some [ [ _ ] ]; [ Some (b, z); Box (Some y) ]; Box (Just [ _ ]) ]; [ [ ((), _, _), (b, a); (c, Nil), Just (Some _); Some (Box Nothing, Some 88) ] ]; Some (Some [ Just (Just y) ]), Some ([], (Box _, Just Nothing), Some (_, y, false)), (([ Nil; _ ], [], Just [ None; y; false ]), Just ([ 5; y; _ ], Just None, [ _ ]), ([], Box (Some z), (true, z, _))) ] ->
z, b)) ]

$ parse -expr << EOF
> [ [ fun (Just Nothing) -> [ true ]; [ [ z ] ]; None ]; [ [ fun [] -> true; fun ([ Box [ _; Some [], [] ] ], (Just [ Some [] ], Box [ [ _ ]; (Some _, [ Nil; _ ]), [ []; Some Nothing ], (Box x, y, Box z); Box [], Just None, ((c, a, b), [ z ], (_, 4)) ], ([], (), Some (Some _, Nothing, [ [ _; Nil; Nil ] ]))), Box (Nothing, (Some Nil, _, Just []))) ->
>   c; fun [ _, _; Box ((), true), [ Just [ true, (z, [ Just Nothing, (y, Nil), Nil ]); [ b; Box (Box [ Just Nothing; Nil ]) ]; 85, ([ Nothing, Some Nothing ], Some ((c, c, 84), Just x, [ Nothing; false; a ]), (Box (None, _, b), (Nil, y, Nothing))), Just (Some Nil) ]; [ Nothing, Box None; Box [ Just (Some ()) ], b, true ]; [ [ y ] ] ], Just (([ ((_, (false, c, _)), [], [ [ (); x ]; Some (); Just Nothing ]), [ _, [ b ], Some None ]; [ [ Just 98 ] ], Nothing; Some c ], c, [ Box x, Just (Some [ z; _; true ]); Just [], Some (Nothing, c), ([ Box _; false, None ], [ Just a; _, true ]); [ Box []; 86, [ true, None, b; a; _ ] ] ]), [ true ]) ] ->
>   a ]; fun Nil -> 53; b ]; fun z ->
>   [ [ Nil; (); () ]; b; fun ([ []; Some ([ Box None, _; _ ], [ [ Just (c, (Box c, (None, _), []), 21); [], [ y, x, [ _; None; None ]; Some Nil, (Nil, _), _ ], _ ]; [] ], [ (); Box (Just (Some [ Nothing, None, Nothing; false, _, false ])); Box false, (((36, [], b), Just [ x ]), y) ]) ], a) ->
>   z ] ]
parsed: [ [ (fun (Just Nothing) -> [ true ]); [ [ z ] ]; None ]; [ [ (fun [] -> true); (fun ([ Box [ _; Some [], [] ] ], (Just [ Some [] ], Box [ [ _ ]; (Some _, [ Nil; _ ]), [ []; Some Nothing ], (Box x, y, Box z); Box [], Just None, ((c, a, b), [ z ], (_, 4)) ], ([], (), Some (Some _, Nothing, [ [ _; Nil; Nil ] ]))), Box (Nothing, (Some Nil, _, Just []))) ->
c); (fun [ _, _; Box ((), true), [ Just [ true, (z, [ Just Nothing, (y, Nil), Nil ]); [ b; Box (Box [ Just Nothing; Nil ]) ]; 85, ([ Nothing, Some Nothing ], Some ((c, c, 84), Just x, [ Nothing; false; a ]), (Box (None, _, b), (Nil, y, Nothing))), Just (Some Nil) ]; [ Nothing, Box None; Box [ Just (Some ()) ], b, true ]; [ [ y ] ] ], Just (([ ((_, (false, c, _)), [], [ [ (); x ]; Some (); Just Nothing ]), [ _, [ b ], Some None ]; [ [ Just 98 ] ], Nothing; Some c ], c, [ Box x, Just (Some [ z; _; true ]); Just [], Some (Nothing, c), ([ Box _; false, None ], [ Just a; _, true ]); [ Box []; 86, [ true, None, b; a; _ ] ] ]), [ true ]) ] ->
a) ]; (fun Nil -> 53); b ]; (fun z ->
[ [ Nil; (); () ]; b; (fun ([ []; Some ([ Box None, _; _ ], [ [ Just (c, (Box c, (None, _), []), 21); [], [ y, x, [ _; None; None ]; Some Nil, (Nil, _), _ ], _ ]; [] ], [ (); Box (Just (Some [ Nothing, None, Nothing; false, _, false ])); Box false, (((36, [], b), Just [ x ]), y) ]) ], a) ->
z) ]) ]
#

# 
$ parse -expr << EOF
> fun _ ->
>   (Nothing * ((fun (Some (Just (Just Nothing)), (Just (Some Nil, Box (_, Some false, _)), None, (((Some (), _, Some (Nil, _)), Some Nil), (_, (Nil, (Box (), (_, Nothing)))))), (((Just _, ((Box (), z, None), Nothing), (Just _, (c, Box ()))), Box a), (true, (true, Just (), Box (Box (Some _))), Box Nil), Box ((Just (_, false), 23), Just ()))) ->
>   (fun Nothing ->
>   fun (Some ((Nothing, _), Box c, true)) ->
>   (fun (Some (Box a)) -> true) - (Nothing, Nil, None)) * ((((fun c -> ()), (fun c -> c)) - (fun ((_, ()), Just true, (Nothing, _, true)) -> 66)) + x)) >= y)) == (Nil >= (((None <> y, true, None / ((fun (Just a, (Just (Just Nothing), Nothing, Box (b, ()))) -> z, 39, false) > (fun (Some (_, Some (Some _), (((Box (Just 100), ((x, c, ()), Some Nil, (Nothing, _, a))), _), Box (Just _), Some Nil))) ->
>   (), true, ()))) <> ((fun b -> fun (None, Box (Some Nil), 1) -> (b >= 29) * (fun (Some ()) -> a)) < ((((fun (Just b) -> Nil), b <= Nothing), Nothing), (Nothing >= (fun (Just ()) -> a), ((a, ()), Nothing), ((Nothing, None, Nil), (fun (Nothing, Just (Box Nothing, (a, 87, Nothing))) -> a))), (a, Nil)))) / (fun (Some (false, Just (Some true))) -> Nil)))

$ parse -expr << EOF
> (((((b, 6, ()), (fun ((((((Nil, None), Nothing), a, y), ((Nil, (_, true, ()), Just a), Nil), Some ()), b, true), Nothing) ->
>   Nothing)) <= Nothing, (fun (((z, _, Just (Some (Just Nil, (Box false, 84, y), (Box z, (z, None, 87), (None, _))), _)), Nothing, Just _), ((Some (Some (Some (Box (Just false))), (Just (Nothing, false, ((), b)), Just (Box (Nil, Nil, ()))), true), Nothing, Some Nothing), (Some None, Just (Just (Just Nil, Just 84), true)), x), Just ((), Box (Some (Box (Nil, a))), Just _)) ->
>   Nothing)), y), (x, ((((), y, 24), Nil >= a, (fun (Just (Just (_, None)), Just (Box b, (false, None))) -> c)) == ((fun a -> None), (12, Nil)), ((fun (Box z, (Just _, ((y, _), Some c, Just (Just None)), 33), true) -> x) <> (6, (), x)) + (fun ((Nil, _), Nothing) -> fun (Some (Some x), Nil) -> c), z)), (51, (((fun (Just _) -> ()) > (y + x)) <= ((y, y, Nothing), (Nil, None)), 48), c > (((fun (Nil, ((a, ((y, Box ((Nothing, _), (Nil, _))), (b, Some (Some ()))), _), Box 53, (_, _, z))) ->
>   ()) < (Nil, None, Nothing)) <= (fun (b, _, y) -> 12 <> ())))), (fun true ->
>   ((42, (None == z) < ((), c, false)), ((Nothing / y) / (Nil, None, false)) <> Nothing) > (fun (Box Nothing) -> (37 == a) < ())), () / Nothin> g

#
