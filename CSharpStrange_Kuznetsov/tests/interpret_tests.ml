(** Copyright 2026, Dmitrii Kuznetsov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open C_sharp_strange_lib.Interpret
open C_sharp_strange_lib.Common

let test_interpret str =
  match interpret str with
  | Result.Ok x ->
    (match x with
     | Some _ -> ()
     | None -> Format.print_string "void\n")
  | Result.Error er -> Format.printf "%a\n%!" pp_error er
;;

let%expect_test "Main 1" =
  test_interpret
    {| 
  class Program {
    static int b = 9;
    static int c = 67;
    static int a;
    static bool r = false;
    static string s = "ok";
    static char h = 'a';
    static bool t;

    public static int Main() { 
      a = (50 % 2) + b - c;
      r = s != "kkkk" && (190%22 == 100 * -2/5);
      t = (a != b * c) || (a >= b) && (a == c +90);
   System.Console.WriteLine(a);
      return a;
    }

  } |};
  [%expect
    {|
    -58 |}]
;;

let%expect_test "Main 2" =
  test_interpret
    {| 
  class Program {
    static int n = 10;
    public static int Main() {
      int res = 0;
      for(int i = 0; i < n; i = i+1) {
        for(int j = 0; j < i; j = j+1) {
          res = res + i *j;
        }
      }

      System.Console.WriteLine(res);
      return res;
    }
  } |};
  [%expect
    {|
    870 |}]
;;

let%expect_test "Main 3" =
  test_interpret
    {| 
  class Program {
    static bool t;
    static int a = 5;

    public static int Main() {
      int b = 5;
      int c = 2;
      t = true;
      if (t) {
        if (t && false) {
          t = false;
          return 1;
        }
        else if( a == b) {
          a = c*67 + 7;
 System.Console.WriteLine(a);
          return a;
      }
      }
      else {
        return 3;
      }
      return 0;
    }
  } |};
  [%expect
    {|
    141 |}]
;;

let%expect_test "Main 4" =
  test_interpret
    {| 
  class Program {
    static int x = 189;
    static int s = 0;
    public static int Main() {
      while (x != 0) {
          s = s + x % 10;
          x = x/ 10;
      }
   System.Console.WriteLine(s);
      return s;
    }
  } |};
  [%expect
    {|
    18 |}]
;;

let%expect_test "Functions 1" =
  test_interpret
    {| 
  class Program {
    public static int is_right_triangle(int a, int b, int c) {
      if ((a + b <= c) || (a + c <= b) || (b + c <= a)) {
          return 0;
      } else if ((a * a + b * b == c * c) || (a * a + c * c == b * b) || (b * b + c * c == a * a)) {
          return 1;
      } else {
          return 2;
      }
    }
    public static int Main() {
   System.Console.WriteLine(is_right_triangle(3,4,5));
      return;
    }
  } |};
  [%expect
    {|
    (TCError TypeMismatch) |}]
;;

let%expect_test "Factorial with writeline" =
  test_interpret
    {|
    class Program {
      int Fac(int num) {
        if (num == 1) {
          return 1;
        }
        else 
        {
          return num * Fac(num - 1);
        }
      }
      public static int Main() {
       int result = Fac(5);
       System.Console.WriteLine(result);
        return result;
      }
    } |};
  [%expect
    {|
    120 |}]
;;
