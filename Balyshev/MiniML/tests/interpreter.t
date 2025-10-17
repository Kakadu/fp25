  $ cat << EOF | ./REPL.exe -eval -expr 
  > 10 + 20 / 4
  "10 + 20 / 4"
  evaluated: 90

  $ cat << EOF | ./REPL.exe -eval -expr 
  > 1, 2 + 3
  "1, 2 + 3"
  evaluated: (1, 5)

  $ cat << EOF | ./REPL.exe -eval -expr 
  > (1 + 2, 3 - 4 * 5)
  "(1 + 2, 3 - 4 * 5)"
  evaluated: (3, -17)

  $ cat << EOF | ./REPL.exe -eval -expr 
  > [ 1 + 2 + 3; 2 * 3; 7 - 1 ]
  "[ 1 + 2 + 3; 2 * 3; 7 - 1 ]"
  evaluated: (6 :: (6 :: (6 :: [])))

  $ cat << EOF | ./REPL.exe -eval -expr 
  > 1 :: 2 :: 3 :: [ 4; 5 ]
  "1 :: 2 :: 3 :: [ 4; 5 ]"
  evaluated: (1 :: (2 :: (3 :: (4 :: (5 :: [])))))

  $ cat << EOF | ./REPL.exe -eval -expr 
  > let (x, y, z) = (6, 15 * 4, 12 * 5 * 10) in x + y + z
  "let (x, y, z) = (6, 15 * 4, 12 * 5 * 10) in x + y + z"
  evaluated: 666

  $ cat << EOF | ./REPL.exe -eval -expr 
  > Some (1 + 2 + 3, 2 * 3, 6)
  "Some (1 + 2 + 3, 2 * 3, 6)"
  evaluated: Some ((6, 6, 6))

  $ cat << EOF | ./REPL.exe -eval -expr 
  > Some [ Just (10 + 20), Just (1 * 2, 3 + 4) ]
  "Some [ Just (10 + 20), Just (1 * 2, 3 + 4) ]"
  evaluated: Some (((Just (30), Just ((2, 7))) :: []))
