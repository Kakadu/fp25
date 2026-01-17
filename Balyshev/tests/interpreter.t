  $ run () { ../bin/REPL.exe -eval "$@"; }

  $ run -expr << EOF
  > 10 + 20 / 4
  evaluated: 15

  $ run -expr << EOF
  > 1, 2 + 3
  evaluated: 1, 5

  $ run -expr << EOF
  > (1 + 2, 3 - 4 * 5)
  evaluated: 3, -17

  $ run -expr << EOF
  > [ 1 + 2 + 3; 2 * 3; 7 - 1 ]
  evaluated: [ 6; 6; 6 ]

  $ run -expr << EOF
  > 1 :: 2 :: 3 :: [ 4; 5 ]
  evaluated: [ 1; 2; 3; 4; 5 ]

  $ run -expr << EOF
  > let (x, y, z) = (6, 15 * 4, 12 * 5 * 10) in x + y + z
  evaluated: 666

  $ run -expr << EOF
  > Some (1 + 2 + 3, 2 * 3, 6)
  evaluated: Some (6, 6, 6)

  $ run -expr << EOF
  > Some [ Just (10 + 20), Just (1 * 2, 3 + 4) ]
  evaluated: Some ([ (Just (30), Just (2, 7)) ])

  $ run -expr << EOF
  > if 1 < 2 then true else false
  evaluated: true

  $ run -expr << EOF
  > let rec fact = fun n -> if n < 2 then 1 else n * fact (n - 1) in fact 5
  evaluated: 120

  $ run -expr << EOF
  > let is_zero x = x == 0 in (is_zero 0, is_zero 1)
  evaluated: true, false

  $ run -expr << EOF
  > let x = 5 in let y = 10 in x + y
  evaluated: 15

  $ run -expr << EOF
  > let id x = x in id 5
  evaluated: 5

  $ run -expr << EOF
  > let apply = fun f x -> f x in
  >   apply (fun x -> x * 6) (100 + 11)
  evaluated: 666

# value binding chains
  $ run -expr << EOF
  > let x = 1 and y = 2 in x + y
  evaluated: 3

  $ run -expr << EOF
  > let Some x = Some 1 and (y, z) = (2, 3) in [ x; y; z ]
  evaluated: [ 1; 2; 3 ]

  $ run -expr << EOF
  > let x = (let y = 1 and z = 2 in y + z) in x + 3
  evaluated: 6

  $ run -expr << EOF
  > (fun x -> let y = x + 1 in y) 5
  evaluated: 6
#

# unsorted
  $ run -expr << EOF
  > let id x = x in
  > (id true, id 1)
  evaluated: true, 1

  $ run -expr << EOF
  > let ite c e1 e2 = if c then e1 else e2 in
  > let first = ite true
  > and second = ite false
  > in (first 1 2) + (second 3 4)
  evaluated: 5

  $ run -expr << EOF
  > let swap (x, y) = (y, x) in
  > let first (x, y) = x in
  > 
  > let x = (1, 2) in first x + first (swap x)
  evaluated: 3

  $ run -expr << EOF
  > let twice f x = f (f x) in
  > let inc x = x + 1 in
  > twice inc 0
  evaluated: 2

  $ run -expr << EOF
  > let apply f x = f x in
  > let inc x = x + 1 in
  > let not x = if x then false else true in
  > (apply inc 0, apply not false)
  evaluated: 1, true

  $ run -expr << EOF
  > let compose f g x = g (f x) in
  > let swap (x, y) = (y, x) in
  > let first (x, y) = x in
  > compose swap first (1, 2)
  evaluated: 2
#

# rec
  $ run -expr<< EOF
  > let rec fact n = if n < 2 then 1 else n * fact (n - 1) in
  > fact 5
  evaluated: 120

  $ run -expr<< EOF
  > let rec fib n = if n < 2 then 1 else fib (n - 1) + fib (n - 2) in
  > fib 5
  evaluated: 8

  $ run -expr<< EOF
  > let abs n = if n < 0 then 0 - n else n in
  > let not b = if b then false else true in
  > let rec is_even_pos n = if n == 0 then true else not (is_even_pos (n - 1)) in
  > let is_even n = is_even_pos (abs n) in
  > 
  > (is_even 1, is_even 2, is_even 3, is_even 4)
  evaluated: false, true, false, true
#

# fix
  $ run -expr << EOF
  > let fact_aux self n = if n < 1 then 1 else n * self (n - 1) in
  > let fact = fix fact_aux in
  > fact 5
  evaluated: 120
#

# print value primitive
  $ run -expr << EOF
  > let (x, y, z) = (6, 6, 6) in
  > printn_value (x + y + z)
  18
  evaluated: ()
#

  $ run -expr << EOF
  > let rec iter f ls =
  >   match ls with
  >   | [] -> ()
  >   | x :: xs ->
  >     let _ = f x in
  >       iter f xs
  > in
  > iter (fun x -> printn_value x) [ 1; 2; 3; 4; 5 ]
  1
  2
  3
  4
  5
  evaluated: ()

  $ run -expr << EOF
  > let rec sum ls =
  >   match ls with
  >   | [] -> 0
  >   | x :: xs -> x + sum xs
  > in
  > printn_value (sum [ 1; 2; 3; 4; 5 ])
  15
  evaluated: ()

  $ run -expr << EOF
  > let rec filter p ls =
  >   match ls with
  >   | [] -> []
  >   | x :: xs ->
  >     if p x then x :: filter p xs else filter p xs
  > in filter (fun x -> x <= 100) [ 1; 10; 100; 1000; 10000 ]
  evaluated: [ 1; 10; 100 ]

  $ run -expr << EOF
  > let rec forall p ls =
  >   match ls with
  >   | [] -> true
  >   | x :: xs -> if p x then forall p xs else false
  > in
  > let lt10 x = x > 10 in
  > let _ = printn_value (forall lt10 [ 0; 5; 10 ]) in
  > let _ = printn_value (forall lt10 [ 15; 20; 25 ]) in ()
  false
  true
  evaluated: ()

  $ run -expr << EOF
  > let not x = if x then false else true in
  > let rec is_even n = if n == 0 then true else (if n > 0 then not (is_even (n - 1)) else not (is_even (n + 1))) in
  > let rec find p ls =
  >   match ls with
  >   | [] -> None
  >   | x :: xs -> if p x then Some x else find p xs
  > in
  > let _ = printn_value (find is_even [ 1; 3; 5; 7; 9 ]) in
  > let _ = printn_value (find is_even [ 1; 1; 2; 3; 5; 8; 13 ]) in ()
  None
  Some (2)
  evaluated: ()

  $ run -expr << EOF
  > let rev ls =
  >  let rec aux ls acc = 
  >     match ls with
  >     | [] -> acc
  >     | x :: xs ->
  >       aux xs (x :: acc)
  >   in aux ls []
  > in
  > rev [ 1; 2; 3 ]
  evaluated: [ 3; 2; 1 ]

  $ run -expr << EOF
  > let rec fold f ls acc =
  >   match ls with
  >   | [] -> acc
  >   | x :: xs -> fold f xs (f acc x)
  > in
  > fold (fun x y -> x * y) [ 1; 2; 3; 4; 5; 6; 7 ] 1
  evaluated: 5040

  $ run -expr << EOF
  > let rec fold f ls acc =
  >   match ls with
  >   | [] -> acc
  >   | x :: xs -> fold f xs (f acc x)
  > in
  > let rec make_naturals n =
  >   if n <= 0 then [] else n :: make_naturals (n - 1)
  > in
  > let fact n = fold (fun x y -> x * y) (make_naturals n) 1 in
  > fact 6
  evaluated: 720

  $ run -expr << EOF
  > let rec take n ls =
  >   match ls with
  >   | [] -> []
  >   | x :: xs -> if n < 1 then [] else x :: take (n - 1) xs
  > in
  > let rec skip n ls =
  >   match ls with
  >   | [] -> []
  >   | x :: xs ->
  >     let tail = skip (n - 1) xs in
  >       if n < 1 then x :: tail else tail
  > in
  > take 3 (skip 2 [ 1; 2; 3; 4; 5; 6; 7 ])
  evaluated: [ 3; 4; 5 ]

  $ run -expr << EOF
  > let sort ls cmp =
  >   let rec insert x ls =
  >     match ls with
  >     | [] -> [ x ]
  >     | y :: ys -> if cmp x y <= 0 then x :: y :: ys else y :: insert x ys
  >   in
  >   let rec aux ls acc =
  >     match ls with
  >     | [] -> acc
  >     | x :: xs -> aux xs (insert x acc)
  >   in
  >   aux ls []
  > in
  > sort [ 2; 3; 4; 1; 6; 7; 5 ] (fun x y -> x - y)
  evaluated: [ 1; 2; 3; 4; 5; 6; 7 ]

  $ run -expr << EOF
  > let len ls =
  >   match ls with
  >   | [] -> 0
  >   | x :: [] -> 1
  >   | x1 :: x2 :: xs -> 2
  > in
  > len [ 1 ]
  evaluated: 1

# match should fail
  $ run -expr << EOF
  > match 2 with
  > | 0 -> 0
  > | 1 -> 1
  interpreter error: match failure

  $ run -expr << EOF
  > match [ 1; 2; 3; 4 ] with
  > | [] -> 0
  > | [ _ ] -> 1
  > | _ :: _ :: [] -> 2
  > | _ :: [ _; _ ] -> 3
  interpreter error: match failure
#
