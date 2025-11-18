  $ INTERPETER="../bin/REPL.exe"

  $ cat << EOF | $INTERPETER -eval -expr 
  > 10 + 20 / 4
  evaluated: 15

  $ cat << EOF | $INTERPETER -eval -expr 
  > 1, 2 + 3
  evaluated: (1, 5)

  $ cat << EOF | $INTERPETER -eval -expr 
  > (1 + 2, 3 - 4 * 5)
  evaluated: (3, -17)

  $ cat << EOF | $INTERPETER -eval -expr 
  > [ 1 + 2 + 3; 2 * 3; 7 - 1 ]
  evaluated: [ 6; 6; 6 ]

  $ cat << EOF | $INTERPETER -eval -expr 
  > 1 :: 2 :: 3 :: [ 4; 5 ]
  parsing error: : end_of_input

  $ cat << EOF | $INTERPETER -eval -expr 
  > let (x, y, z) = (6, 15 * 4, 12 * 5 * 10) in x + y + z
  evaluated: 666

  $ cat << EOF | $INTERPETER -eval -expr 
  > Some (1 + 2 + 3, 2 * 3, 6)
  evaluated: Some ((6, 6, 6))

  $ cat << EOF | $INTERPETER -eval -expr 
  > Some [ Just (10 + 20), Just (1 * 2, 3 + 4) ]
  evaluated: Some ([ (Just (30), Just ((2, 7))) ])

  $ cat << EOF | $INTERPETER -eval -expr 
  > if 1 < 2 then true else false
  evaluated: true

  $ cat << EOF | $INTERPETER -eval -expr 
  > let rec fact = fun n -> if n < 2 then 1 else n * fact (n - 1) in fact 5
  interpreter error: not implemented in <let rec in eval_expr>

  $ cat << EOF | $INTERPETER -eval -expr 
  > let is_zero = fun x -> if x = 0 then true else false in is_zero 1
  parsing error: : char '['

  $ cat << EOF | $INTERPETER -eval -expr 
  > let x = 5 in let y = 10 in x + y
  evaluated: 15

  $ cat << EOF | $INTERPETER -eval -expr 
  > let f = (fun x -> x) in
  >   let arg = 5 in
  >     let apply = fun f arg -> f arg in
  >       apply f arg
  evaluated: 5

  $ cat << EOF | $INTERPETER -eval -expr 
  > let apply = fun f x -> f x in
  >   apply (fun x -> x * 6) (100 + 11)
  evaluated: 666

# value binding chains
  $ cat << EOF | $INTERPETER -eval -expr 
  > let x = 1 and y = 2 in x + y
  evaluated: 3

  $ cat << EOF | $INTERPETER -eval -expr 
  > let Some x = Some 1 and (y, z) = (2, 3) in [ x; y; z ]
  evaluated: [ 1; 2; 3 ]

  $ cat << EOF | $INTERPETER -eval -expr 
  > let x = (let y = 1 and z = 2 in y + z) in x + 3
  evaluated: 6

  $ cat << EOF | $INTERPETER -eval -expr 
  > (fun x -> let y = x + 1 in y) 5
  evaluated: 6
#

# unsorted
  $ cat << EOF | $INTERPETER -eval -expr 
  > let ite = fun c e1 e2 -> if c then e1 else e2 in
  > let first = ite true
  > and second = ite false
  > in (first 1 2) + (second 3 4)
  evaluated: 5
#

# recursiveness
  $ cat << EOF | $INTERPETER -parse -stru
  > let rec fact n = if n < 2 then 1 else n * fact (n - 1)
  parsed: let rec fact = (fun n -> if (n < 2) then 1 else (n * fact (n - 1)))
#
