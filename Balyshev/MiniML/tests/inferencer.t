  $ INTERPETER="../bin/REPL.exe"

  $ cat << EOF | $INTERPETER -infer -expr 
  > 10 + 20 / 4
  inferred: int

  $ cat << EOF | $INTERPETER -infer -expr 
  > 1 = 2
  inferred: bool

  $ cat << EOF | $INTERPETER -infer -expr 
  > (1 = 2) + 3
  inferencer error: unification of bool and int failed

  $ cat << EOF | $INTERPETER -infer -expr 
  > (1 + 2, 3 = 4, true)
  inferred: (int, bool, bool)

  $ cat << EOF | $INTERPETER -infer -expr 
  > fun x -> x
  inferred: ('gen_0 -> 'gen_0)

  $ cat << EOF | $INTERPETER -infer -expr 
  > (fun x -> x + 1)
  inferred: (int -> int)

  $ cat << EOF | $INTERPETER -infer -expr 
  > (fun x -> x + 1) 2
  inferred: int

  $ cat << EOF | $INTERPETER -infer -expr 
  > fun x y -> x + y
  inferred: (int -> (int -> int))

  $ cat << EOF | $INTERPETER -infer -expr 
  > (fun f -> fun x -> f x)
  inferred: (('gen_1 -> 'gen_2) -> ('gen_1 -> 'gen_2))

  $ cat << EOF | $INTERPETER -infer -expr 
  > (fun x -> x x)
  inferencer error: 'gen_0 occurs in ('gen_0 -> 'gen_1)

  $ cat << EOF | $INTERPETER -infer -expr 
  > (fun f g x -> f)
  inferred: ('gen_0 -> ('gen_1 -> ('gen_2 -> 'gen_0)))

  $ cat << EOF | $INTERPETER -infer -expr 
  > (fun f g x -> f x + g x)
  inferred: ('gen_0 -> ('gen_1 -> ('gen_2 -> int)))
