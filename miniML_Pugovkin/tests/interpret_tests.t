  $ ../bin/REPL.exe <<'EOF'
  > let rec fact n = if n then n * fact (n - 1) else 1;;
  > fact 5
  > EOF
  120

  $ ../bin/REPL.exe <<'EOF'
  > let rec fib n = if n <= 1 then n else fib (n - 1) + fib (n - 2);;
  > fib 10
  > EOF
  55

  $ ../bin/REPL.exe <<'EOF'
  > let fact = fix (fun f -> fun n -> if n then n * f (n - 1) else 1);;
  > fact 6
  > EOF
  720

  $ ../bin/REPL.exe <<'EOF'
  > print_int 7;;
  > 42
  > EOF
  7
  ()
  42

  $ ../bin/REPL.exe -steps 50 <<'EOF'
  > let rec loop x = loop x;;
  > loop 0
  > EOF
  Runtime error: step limit exceeded
  [1]
