  $ cat test_data/test1.txt
  let x = 40 + 2 ;;
  let () = print_int x
  
  $ main test_data/test1.txt
  42

  $ cat test_data/test2.txt
  let rec fac n = if n then n * fac (n - 1) else 1 ;;
  let () = print_int (fac 10)

  $ main test_data/test2.txt
  3628800

  $ cat test_data/test3.txt
  let rec fix f x = f (fix f) x ;;
  let fact = fix (fun slf n -> if n then n * slf (n - 1) else 1) ;;
  let () = print_int (fact 10)

  $ main test_data/test3.txt
  3628800

  $ cat test_data/test4.txt
  let fib n =
    let rec fib_iter a b count = if count then fib_iter b (a + b) (count - 1) else a in
    fib_iter 0 1 n
  ;;
  let () = print_int (fib 9)

  $ main test_data/test4.txt
  34

  $ cat test_data/test5.txt
  let div x y = x / y ;;
  let () = print_int (div 20 0)

  $ main test_data/test5.txt
  Error: Division by zero

  $ cat test_data/test6.txt
  let rec rec x = 42

  $ main test_data/test6.txt
  Parse Error: : end_of_input

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

