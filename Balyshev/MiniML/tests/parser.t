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
  > [ a; (b, c); d + e ]
  parsed: (a :: ((b, c) :: ((d + e) :: [])))

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
  parsed: f (x, y) (a + b) (q :: (w :: (e :: [])))

  $ cat << EOF | $INTERPETER -parse -expr 
  > f (g x) (h y z)
  parsed: f g x h y z

  $ cat << EOF | $INTERPETER -parse -expr 
  > [ a ] [ b ]
  parsed: (a :: []) (b :: [])

# lambda

  $ cat << EOF | $INTERPETER -parse -expr 
  > fun _ -> 42
  parsed: fun _ -> 42

  $ cat << EOF | $INTERPETER -parse -expr 
  > fun (x, y) -> x
  parsed: fun (x, y) -> x

  $ cat << EOF | $INTERPETER -parse -expr 
  > fun x y -> x + y
  parsed: fun x -> fun y -> (x + y)

  $ cat << EOF | $INTERPETER -parse -expr 
  > (fun x -> x, fun x -> x + 1)
  parsed: fun x -> (x, fun x -> (x + 1))

  $ cat << EOF | $INTERPETER -parse -expr 
  > (fun x -> x) (fun x -> x + 1)
  parsing error: : end_of_input

  $ cat << EOF | $INTERPETER -parse -expr 
  > map (fun x -> x) items
  parsed: map fun x -> x items

  $ cat << EOF | $INTERPETER -parse -expr 
  > fun x -> fun y -> y
  parsed: fun x -> fun y -> y

  $ cat << EOF | $INTERPETER -parse -expr 
  > [ fun x -> x; fun x -> x + 1 ]
  parsed: (fun x -> x :: (fun x -> (x + 1) :: []))

  $ cat << EOF | $INTERPETER -parse -expr 
  > map (fun x -> x) (fun y -> y)
  parsed: map fun x -> x fun y -> y

# if then else

  $ cat << EOF | $INTERPETER -parse -expr 
  > if 1 then 1 else 0
  parsed: if 1 then 1 else 0

  $ cat << EOF | $INTERPETER -parse -expr 
  > if (a < b) then (a + b) else (a - b)
  parsed: if (a < b) then (a + b) else (a - b)

  $ cat << EOF | $INTERPETER -parse -expr 
  > if (f x) then (fun f -> f x) else (fun x -> f x)
  parsed: if f x then fun f -> f x else fun x -> f x

  $ cat << EOF | $INTERPETER -parse -expr 
  > let rec fact = fun n -> if n < 2 then 1 else n * fact (n - 1) in fact 5
  parsed: let rec fact = fun n -> if (n < 2) then 1 else (n * fact (n - 1)) in fact 5
