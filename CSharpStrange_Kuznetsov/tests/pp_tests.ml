(** Copyright 2026, Dmitrii Kuznetsov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open C_sharp_strange_lib.Prettyprinter
open C_sharp_strange_lib.Parser
open Format

(* Debug (TODO remove later) 
let test_pp name source =
  let prog = parse_option parse_prog source in
  let pretty =
    match prog with
    | Some x -> asprintf "%a" pp_prog x
    | None -> ""
  in
  let prog_after_pp = parse_option parse_prog pretty in
  if prog = prog_after_pp
  then Printf.printf "✓ %s: roundtrip successful\n" name
  else Printf.printf "✗ %s: roundtrip failed\n" name;
  prog = prog_after_pp
;;
*)

let test_pp _ source =
  let prog = parse_option parse_prog source in
  let pretty =
    match prog with
    | Some x -> asprintf "%a" pp_prog x
    | None -> ""
  in
  let prog_after_pp = parse_option parse_prog pretty in
  prog = prog_after_pp
;;

let samples =
  [ ( "Factorial"
    , {|
public class Program
{
    public int Factorial(int n)
    {
        if (n == 0)
        {
            return 1;
        }
        else
        {
            return n * Factorial(n - 1);
        }
    }

    public static void Main()
    {
    }
}
|}
    )
  ; ( "Cycles 1"
    , {|
public class Program
{
    public int Cycles(int n, bool e, string x)
    {
        int x = 0;
        while (x < n)
        {
            if (x == -1)
            {
                break;
            }

            if (x == -2)
            {
                continue;
            }

            x = x + 1;
        }

        for (int i = 1; i < n; i++)
        {
            break;
        }

        for (;;)
        {
            break;
        }

        for (int i = 1;; i++)
        {
            break;
        }
    }

    public static void Main()
    {
        Cycles(5, true, "sample");
    }
}
|}
    )
  ; ( "Binops 1"
    , {|
public class Program
{
    public int Binops(int n, bool e, string x)
    {
        int x_ = n;
        bool sample = !e || ((1 + 2 < 3 + 4) && (5 == 8));
        string e = x;
        char eeAe065ef = 'a';
        e = null;
        const int a = 1;
    }

    public static void Main()
    {
        Binops(5, true, "");
    }
}
|}
    )
  ; ( "StaticClass"
    , {|
public static class Program {
    static int result = 0;
    
    public static void Main(string[] args) {
        int a = 5;
        int b = 3;
        result = a + b * 2;
        
        if (result > 10) {
            result = result - 10;
        }
        
        return;
    }
}
|}
    )
  ; ( "EmptyClass"
    , {|
public static class Program {
    public static void Main() {
        {
            {
                
            }
        }
    }
}
|}
    )
  ; ( "MultipleFields"
    , {|
public class Test {
    int a, b, c;
    static string x, y;
    const int MAX = 100;
}
|}
    )
  ; ( "Simple arithmetic"
    , {|
public static class Program {
    static int result = 0;
    
    public static void Main(string[] args) {
        int a = 5;
        int b = 3;
        result = a + b * 2;
        
        if (result > 10) {
            result = result - 10;
        }
        
        return;
    }
}
|}
    )
  ; ( "Cycles 2"
    , {|
public static class Program {
    static int sum = 0;
    
    public static void Main(string[] args) {
        int i = 0;
        
        while (i < 5) {
            sum = sum + i;
            i = i + 1;
        }
        
        for (int j = 0; j < 3; j = j + 1) {
            sum = sum + j;
        }
        
        return;
    }
}
|}
    )
  ; ( "Boolean"
    , {|
public static class Program {
    static bool flag = true;
    static int value = 42;
    
    public static void Main(string[] args) {
        bool condition = flag && (value > 40);
        
        if (condition) {
            value = 100;
        } else {
            value = 0;
        }
        
        if (value == 100) {
            flag = false;
        }
        
        return;
    }
}
|}
    )
  ; ( "Strings & chars"
    , {|
public static class Program {
    static string message = "Hello";
    static char symbol = 'A';
    
    public static void Main(string[] args) {
        string name = "World";
        string result = message + " " + name;
        
        char nextSymbol = symbol + 1;
        
        if (result != "Hello World") {
            result = "Error";
        }
        
        return;
    }
}
|}
    )
  ; ( "Cycles 3"
    , {|
public static class Program {
    static int counter = 0;
    
    public static void Main(string[] args) {
        for (int i = 0; i < 10; i = i + 1) {
            if (i == 3) {
                continue;
            }
            
            counter = counter + 1;
            
            if (counter > 5) {
                break;
            }
            
            {
                int temp = counter * 2;
                counter = temp;
            }
        }
        
        return;
    }
}
|}
    )
  ; ( "Complex exprs"
    , {|
public static class Program {
    static int x = 10;
    static int y = 20;
    static bool ok = true;
    
    public static void Main(string[] args) {
        int result = (x + y) * (x - y) / 2;
        
        bool check = (x > y) && ok || (x <= y);
        
        if (!check && result != 0) {
            result = -result;
        }
        
        return;
    }
}
|}
    )
  ; ( "Multiple definitions"
    , {|
public static class Program {
    static int a = 1;
    static int b = 2;
    static string s1 = "first";
    static string s2 = "second";
    static bool b1 = true;
    static bool b2 = false;
    static char c1 = 'X';
    static char c2 = 'Y';
    
    public static void Main(string[] args) {
        int x = a + b;
        string text = s1 + s2;
        bool flag = b1 && b2;
        char letter = c1;
        
        return;
    }
}
|}
    )
  ; ( "Binops 2"
    , {|
public static class Program {
    static int value = 100;
    
    public static void Main(string[] args) {
        int a = 5;
        int b = 3;
        
        int sum = a + b;
        int sub = a - b;
        int mul = a * b;
        int div = a / b;
        int mod = a % b;
        
        bool eq = a == b;
        bool neq = a != b;
        bool lt = a < b;
        bool gt = a > b;
        bool lte = a <= b;
        bool gte = a >= b;
        
        bool and = true && false;
        bool or = true || false;
        bool not = !true;
        
        int neg = -a;
        
        return;
    }
}
|}
    )
  ]
;;

let%test "All pp roundtrip tests" =
  List.for_all (fun (name, source) -> test_pp name source) samples
;;

(* TODO: check string[] args later!! *)

(* TODO: simple arrays:

public static class Program {
    static int[] numbers = null;
    
    public static void Main(string[] args) {
        int index = 0;
        int value = numbers[index];
        
        numbers[index + 1] = value * 2;
        
        return;
    }
}
*)
