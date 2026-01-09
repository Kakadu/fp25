  $ ./mini_cli.exe <<'EOF'
  > let rec fact = fun n -> if n = 1 then 1 else n * fact (n-1) in fact 4
  > EOF
  Result: 24

  $ ./mini_cli.exe <<'EOF'
  > fix (fun fib -> fun n -> if n = 0 then 0 else if n = 1 then 1 else fib (n-1) + fib (n-2)) 6
  > EOF
  Result: 8

  $ ./mini_cli.exe <<'EOF'
  > fix (fun f -> fun n -> f n) 0
  > EOF
  Error: Not a value: Steps limit exceeded
