  $ cat test_data/test1.txt
  let x = 40 + 2 ;;
  let () = print_int x
  
  $ main test_data/test1.txt
  42

  $ ocaml test_data/test1.txt
  42

  $ cat test_data/test2.txt
  let rec fac n = if n<>0 then n * fac (n - 1) else 1 ;;
  let () = print_int (fac 10)

  $ main test_data/test2.txt
  3628800

  $ ocaml test_data/test2.txt
  3628800

  $ cat test_data/test3.txt
  let rec fix f x = f (fix f) x ;;
  let fact = fix (fun slf n -> if n<>0 then n * slf (n - 1) else 1) ;;
  let () = print_int (fact 10)

  $ main test_data/test3.txt
  3628800

  $ ocaml test_data/test3.txt
  3628800

  $ cat test_data/test4.txt
  let fib n =
    let rec fib_iter a b count = if count<>0 then fib_iter b (a + b) (count - 1) else a in
    fib_iter 0 1 n
  ;;
  let () = print_int (fib 9)

  $ main test_data/test4.txt
  34

  $ ocaml test_data/test4.txt
  34

  $ cat test_data/test5.txt
  let div x y = x / y ;;
  let () = print_int (div 20 0)

  $ main test_data/test5.txt
  Error: Division by zero

  $ ocaml test_data/test5.txt
  Exception: Division_by_zero.
  [2]

  $ cat test_data/test6.txt
  let rec rec x = 42

  $ main test_data/test6.txt
  Parse Error: : end_of_input

  $ ocaml test_data/test6.txt
  File "./test_data/test6.txt", line 1, characters 8-11:
  1 | let rec rec x = 42
              ^^^
  Error: Syntax error
  [2]

  $ main test
  File error: test: No such file or directory
  [1]

  $ main test_data/test2.txt --maxsteps=10
  Error: Out of maximum allowed evaluation steps

  $ cat test_data/test7.txt
  let rec fib = fun n ->
    if n < 2 then n else fib (n - 1) + fib (n - 2) ;;
  let () = print_int (fib 15)

  $ main test_data/test7.txt
  610

  $ ocaml test_data/test7.txt
  610

  $ cat test_data/test8.txt
  let f x y = x + y ;;
  let () = print_int(f 1 true);;
  $ main test_data/test8.txt
  Type Error: Unify: int vs bool

  $ ocaml test_data/test8.txt
  File "./test_data/test8.txt", line 2, characters 23-27:
  2 | let () = print_int(f 1 true);;
                             ^^^^
  Error: This expression has type bool but an expression was expected of type
           int
  [2]

  $ cat test_data/test9.txt
  class fact = 
  object(self)
  method factorial n = if n > 0 then n * self#factorial (n-1) else 1
  end ;;
  
  let a = new fact ;;
  let () = print_int (a#factorial 10) ;;

  $ main test_data/test9.txt
  3628800

  $ ocaml test_data/test9.txt
  3628800


  $ cat test_data/test10.txt
  class point x y =
  object
  
  val x = x
  val y = y 
  method p = x*x + y*y
  method move (x1,x2) = new point (x + x1) (y + x2)
  end;;
  
  let a = new point 5 5 ;;
  let b = a#move (4,3) ;;
  let () = print_int b#p ;;


  $ main test_data/test10.txt
  145

  $ ocaml test_data/test10.txt
  145

