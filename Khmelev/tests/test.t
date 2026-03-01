Copyright 2021-2024, Kakadu and contributors
SPDX-License-Identifier: CC0-1.0

Basic arithmetic
  $ cat cram_test/test_arithmetic.txt
  2 + 3
  $ ../bin/REPL.exe cram_test/test_arithmetic.txt
  5

Let binding
  $ cat cram_test/test_let_binding.txt
  let x = 5 in let y = 3 in (x + y)
  $ ../bin/REPL.exe cram_test/test_let_binding.txt
  8

Function application
  $ cat cram_test/test_function.txt
  let add x y = (x + y) in ((add 3) 5)
  $ ../bin/REPL.exe cram_test/test_function.txt
  8

Factorial
  $ cat cram_test/test_factorial.txt
  let rec fact n = if (n > 0) then ((fact (n - 1)) * n) else 1 in (println_int (fact 5))
  $ ../bin/REPL.exe cram_test/test_factorial.txt
  120
  120

Fibonacci
  $ cat cram_test/test_fibonacci.txt
  let rec fib n = if (n <= 1) then n else ((fib (n - 1)) + (fib (n - 2))) in (println_int (fib 10))
  $ ../bin/REPL.exe cram_test/test_fibonacci.txt
  55
  55

Ackermann function
  $ cat cram_test/test_ackermann.txt
  let rec ackermann m n = if (m = 0) then (n + 1) else (if (n = 0) then (ackermann (m - 1) 1) else (ackermann (m - 1) (ackermann m (n - 1)))) in (ackermann 2 3)
  $ ../bin/REPL.exe cram_test/test_ackermann.txt
  9

Comparison operators
  $ cat cram_test/test_comparison.txt
  if (3 < 5) then 10 else 20
  $ ../bin/REPL.exe cram_test/test_comparison.txt
  10

Division by zero error
  $ cat cram_test/test_division_by_zero.txt
  10 / 0
  $ ../bin/REPL.exe cram_test/test_division_by_zero.txt
  Error: Division by zero

Unknown variable error
  $ cat cram_test/test_unknown_var.txt
  x + 5
  $ ../bin/REPL.exe cram_test/test_unknown_var.txt
  Error: Unknown variable: x

Sum of numbers 1 to 10
  $ cat cram_test/test_sum.txt
  let rec sum n = if (n <= 0) then 0 else (n + (sum (n - 1))) in (sum 10)
  $ ../bin/REPL.exe cram_test/test_sum.txt
  55

Max function
  $ cat cram_test/test_max.txt
  let max x y = if (x > y) then x else y in ((max 10) 7)
  $ ../bin/REPL.exe cram_test/test_max.txt
  10

Infinite loop with step limit
  $ cat cram_test/test_infinite_loop.txt
  let rec loop x = (loop x) in (loop 1)
  $ ../bin/REPL.exe cram_test/test_infinite_loop.txt --maxSteps 100
  Error: Step limit reached

Type error
  $ cat cram_test/test_type_error.txt
  (fun x -> x) + 5
  $ ../bin/REPL.exe cram_test/test_type_error.txt
  Error: Type error: binary operation expects two integers
