(** Copyright 2026, Dmitrii Kuznetsov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open C_sharp_strange_lib.Interpret
open C_sharp_strange_lib.Common

let show_wrap str =
  match interpret str with
  | Result.Ok x ->
    (match x with
     | Some x -> Format.printf "Result: '%a'" pp_value x
     | None -> Format.print_string "Result: void\n")
  | Result.Error er -> Format.printf "%a\n%!" pp_error er
;;

(* TODO: include TC? *)

let%expect_test "Main 1" =
  show_wrap
    {| 
  class Program {
    static int b = 9;
    static int c = 67;
    static int a;
    static bool r = false;
    static string s = "ok";
    static char h = 'a';
    static bool t;

    static int Main() { 
      a = (50 % 2) + b - c;
      r = s != "kkkk" && (190%22 == 100 * -2/5);
      t = (a != b * c) || (a >= b) && (a == c +90);
      return a;
    }

  } |};
  [%expect
    {|
    Result: '-58' |}]
;;

(* TODO: Access to non-static fields from static methods is prohibited
   Static classes can only have static methods, but static cannot be the program entry point
   Maybe add class check inside main, but won't have time
*)

let%expect_test "Main 2" =
  show_wrap
    {| 
  class Program {
    static int n = 10;
    static int Main() {
      int res = 0;
      for(int i = 0; i < n; i = i+1) {
        for(int j = 0; j < i; j = j+1) {
          res = res + i *j;
        }
      }
      return res;
    }
  } |};
  [%expect
    {|
    Result: '870' |}]
;;

(* TODO: n without static *)

let%expect_test "Main 3" =
  show_wrap
    {| 
  class Program {
    static bool t;
    static int a = 5;

    static int Main() {
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
    Result: '141' |}]
;;

let%expect_test "Main 4" =
  show_wrap
    {| 
  class Program {
    static int x = 189;
    static int s = 0;
    static int Main() {
      while (x != 0) {
          s = s + x % 10;
          x = x/ 10;
      }
      return s;
    }
  } |};
  [%expect
    {|
    Result: '18' |}]
;;

let%expect_test "Functions 1" =
  show_wrap
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
    static int Main() {
      return is_right_triangle(3,4,5);
    }
  } |};
  [%expect
    {|
    Result: '1' |}]
;;

(* TODO: non static not allowed *)
