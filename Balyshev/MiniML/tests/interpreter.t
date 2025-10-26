  $ cat << EOF | ./REPL.exe -eval -expr 
  > 10 + 20 / 4
  evaluated: 15

  $ cat << EOF | ./REPL.exe -eval -expr 
  > 1, 2 + 3
  evaluated: (1, 5)

  $ cat << EOF | ./REPL.exe -eval -expr 
  > (1 + 2, 3 - 4 * 5)
  evaluated: (3, -17)

  $ cat << EOF | ./REPL.exe -eval -expr 
  > [ 1 + 2 + 3; 2 * 3; 7 - 1 ]
  evaluated: (6 :: (6 :: (6 :: [])))

  $ cat << EOF | ./REPL.exe -eval -expr 
  > 1 :: 2 :: 3 :: [ 4; 5 ]
  evaluated: (1 :: (2 :: (3 :: (4 :: (5 :: [])))))

  $ cat << EOF | ./REPL.exe -eval -expr 
  > let (x, y, z) = (6, 15 * 4, 12 * 5 * 10) in x + y + z
  evaluated: 666

  $ cat << EOF | ./REPL.exe -eval -expr 
  > Some (1 + 2 + 3, 2 * 3, 6)
  evaluated: Some ((6, 6, 6))

  $ cat << EOF | ./REPL.exe -eval -expr 
  > Some [ Just (10 + 20), Just (1 * 2, 3 + 4) ]
  evaluated: Some (((Just (30), Just ((2, 7))) :: []))

  $ cat << EOF | ./REPL.exe -eval -expr 
  > if 1 < 2 then true else false
  evaluated: true

  $ cat << EOF | ./REPL.exe -eval -expr 
  > let rec fact = fun n -> if n < 2 then 1 else n * fact (n - 1) in fact 5
  interpreter error: not implemented in <eval_expression>

  $ cat << EOF | ./REPL.exe -eval -expr 
  > let is_zero = fun x -> if x = 0 then true else false in is_zero 1
  evaluated: false

  $ cat << EOF | ./REPL.exe -eval -expr 
  > let x = 5 in let y = 10 in x + y
  evaluated: 15
