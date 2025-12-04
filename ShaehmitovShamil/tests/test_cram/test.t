  $ main test_data/test1.txt
  42

  $ main test_data/test2.txt
  3628800

  $ main test_data/test3.txt
  3628800

  $ main test_data/test4.txt
  34

  $ main test_data/test5.txt
  Error: Division by zero

  $ main test_data/test6.txt
  Parse Error: : end_of_input

  $ main test
  File error: test: No such file or directory
  [1]

  $ main test_data/test2.txt --maxsteps=10
  Error: Out of maximum allowed evaluation steps

