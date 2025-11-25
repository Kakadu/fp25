(** Copyright 2025, Tenyaeva Ekaterina *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

  $ ../bin/REPL.exe -dparsetree <<EOF
  > 122
  [(Str_eval (Expr_const (Const_int 122)))]

  $ ../bin/REPL.exe -dparsetree <<EOF
  > (x : int)
  [(Str_eval (Expr_constraint (Type_int, (Expr_ident "x"))))]

  $ ../bin/REPL.exe -dparsetree <<EOF
  > let x = Some y
  [(Str_value (NonRecursive,
      { vb_pat = (Pat_var "x"); vb_expr = (Expr_option (Some (Expr_ident "y")))
        },
      []))
    ]

  $ ../bin/REPL.exe -dparsetree <<EOF
  > let x = 5 and y = 6
  [(Str_value (NonRecursive,
      { vb_pat = (Pat_var "x"); vb_expr = (Expr_const (Const_int 5)) },
      [{ vb_pat = (Pat_var "y"); vb_expr = (Expr_const (Const_int 6)) }]))
    ]

  $ ../bin/REPL.exe -dparsetree <<EOF
  > if x then if y then z
  [(Str_eval
      (Expr_if ((Expr_ident "x"),
         (Expr_if ((Expr_ident "y"), (Expr_ident "z"), None)), None)))
    ]

  $ ../bin/REPL.exe -dparsetree <<EOF
  > match x with | Some x -> y | None -> z
  [(Str_eval
      (Expr_match ((Expr_ident "x"),
         { case_pat = (Pat_option (Some (Pat_var "x")));
           case_expr = (Expr_ident "y") },
         [{ case_pat = (Pat_option None); case_expr = (Expr_ident "z") }])))
    ]


  $ ../bin/REPL.exe -dparsetree <<EOF
  > function | x -> y | z -> w
  [(Str_eval
      (Expr_function (
         { case_pat = (Pat_var "x"); case_expr = (Expr_ident "y") },
         [{ case_pat = (Pat_var "z"); case_expr = (Expr_ident "w") }])))
    ]

