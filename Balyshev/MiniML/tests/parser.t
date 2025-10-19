  $ cat << EOF | ./REPL.exe -parse -expr 
  > 10 + 20 / 4
  "10 + 20 / 4"
  parsed: (10 + (20 / 4))

  $ cat << EOF | ./REPL.exe -parse -expr 
  > x * y + (z - w)
  "x * y + (z - w)"
  parsed: ((x * y) + (z - w))

  $ cat << EOF | ./REPL.exe -parse -expr 
  > (1, (2, 3), (4, 5, 6))
  "(1, (2, 3), (4, 5, 6))"
  parsed: (1, (2, 3), (4, 5, 6))

  $ cat << EOF | ./REPL.exe -parse -expr 
  > Just (Some None)
  "Just (Some None)"
  parsed: Just (Some (None))

  $ cat << EOF | ./REPL.exe -parse -expr 
  > Just (x + y + z)
  "Just (x + y + z)"
  parsed: Just (((x + y) + z))

  $ cat << EOF | ./REPL.exe -parse -expr 
  > Just (x, y)
  "Just (x, y)"
  parsed: Just ((x, y))

  $ cat << EOF | ./REPL.exe -parse -expr 
  > [ a; (b, c); d + e ]
  "[ a; (b, c); d + e ]"
  parsed: (a :: ((b, c) :: ((d + e) :: [])))

  $ cat << EOF | ./REPL.exe -parse -expr 
  > let x = 5 in x + 1
  "let x = 5 in x + 1"
  parsed: let x = 5 in (x + 1)

  $ cat << EOF | ./REPL.exe -parse -expr 
  > let _ = 5 in 5
  "let _ = 5 in 5"
  parsed: let _ = 5 in 5

  $ cat << EOF | ./REPL.exe -parse -expr 
  > let (x, y, z) = (1, 2, 3) in x + y + z
  "let (x, y, z) = (1, 2, 3) in x + y + z"
  parsed: let (x, y, z) = (1, 2, 3) in ((x + y) + z)

  $ cat << EOF | ./REPL.exe -parse -expr 
  > let (x, y, z) = (1, 2, 3) in x + y + z
  "let (x, y, z) = (1, 2, 3) in x + y + z"
  parsed: let (x, y, z) = (1, 2, 3) in ((x + y) + z)

# application
  $ cat << EOF | ./REPL.exe -parse -expr 
  > f x
  "f x"
  parsed: f x

  $ cat << EOF | ./REPL.exe -parse -expr 
  > f (x, y) (a + b) [q; w; e]
  "f (x, y) (a + b) [q; w; e]"
  parsed: f (x, y) (a + b) (q :: (w :: (e :: [])))

  $ cat << EOF | ./REPL.exe -parse -expr 
  > f (g x) (h y z)
  "f (g x) (h y z)"
  parsed: f g x h y z

# lambda

  $ cat << EOF | ./REPL.exe -parse -expr 
  > fun _ -> 42
  "fun _ -> 42"
  parsed: fun _ -> 42

  $ cat << EOF | ./REPL.exe -parse -expr 
  > fun (x, y) -> x
  "fun (x, y) -> x"
  parsed: fun (x, y) -> x

  $ cat << EOF | ./REPL.exe -parse -expr 
  > fun x y -> y
  "fun x y -> y"
  parsed: fun x -> fun y -> y
