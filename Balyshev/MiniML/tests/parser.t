  $ cat << EOF | ./REPL.exe -parse -expr 
  > 10 + 20 / 4
  "10 + 20 / 4"
  parsed: (10 + (20 / 4))

  $ cat << EOF | ./REPL.exe -parse -expr 
  > (1, (2, 3), (4, 5, 6))
  "(1, (2, 3), (4, 5, 6))"
  parsed: (1, (2, 3), (4, 5, 6))

  $ cat << EOF | ./REPL.exe -parse -expr 
  > Just (Some None)
  "Just (Some None)"
  parsed: Just (Some (None))

  $ cat << EOF | ./REPL.exe -parse -expr 
  > [ a; (b, c); d + e ]
  "[ a; (b, c); d + e ]"
  parsed: (a :: ((b, c) :: ((d + e) :: [])))

  $ cat << EOF | ./REPL.exe -parse -expr 
  > let x = 5 in x + 1
  "let x = 5 in x + 1"
  parsed: let x = 5 in (x + 1)

  $ cat << EOF | ./REPL.exe -parse -expr 
  > let (x, y, z) = (1, 2, 3) in x + y + z
  "let (x, y, z) = (1, 2, 3) in x + y + z"
  parsed: let (x, y, z) = (1, 2, 3) in ((x + y) + z)

  $ cat << EOF | ./REPL.exe -parse -expr 
  > let (x, y, z) = (1, 2, 3) in x + y + z
  "let (x, y, z) = (1, 2, 3) in x + y + z"
  parsed: let (x, y, z) = (1, 2, 3) in ((x + y) + z)
