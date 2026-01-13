(** Copyright 2025, Tenyaeva Ekaterina *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

  $ ../bin/REPL.exe -dparsetree <<EOF
  > 122
  [(Str_eval (Expr_const (Const_int 122)))]

  $ ../bin/REPL.exe -dparsetree <<EOF
  > (x : int)
  [(Str_eval (Expr_constraint (Type_int, (Expr_ident "x"))))]

  $ ../bin/REPL.exe -dparsetree <<EOF
  > (x : int option)
  [(Str_eval (Expr_constraint ((Type_option Type_int), (Expr_ident "x"))))]

  $ ../bin/REPL.exe -dparsetree <<EOF
  > (x : int -> int)
  [(Str_eval
      (Expr_constraint ((Type_arrow (Type_int, Type_int)), (Expr_ident "x"))))
    ]

  $ ../bin/REPL.exe -dparsetree <<EOF
  > (x : (int -> int) option)
  [(Str_eval
      (Expr_constraint ((Type_option (Type_arrow (Type_int, Type_int))),
         (Expr_ident "x"))))
    ]

  $ ../bin/REPL.exe -dparsetree <<EOF
  > (x : int -> int -> int -> int option)
  [(Str_eval
      (Expr_constraint (
         (Type_arrow (Type_int,
            (Type_arrow (Type_int,
               (Type_arrow (Type_int, (Type_option Type_int)))))
            )),
         (Expr_ident "x"))))
    ]

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
  > let x = 5 and y = 6 and z = 7 in x + y + z
  [(Str_eval
      (Expr_let (NonRecursive,
         { vb_pat = (Pat_var "x"); vb_expr = (Expr_const (Const_int 5)) },
         [{ vb_pat = (Pat_var "y"); vb_expr = (Expr_const (Const_int 6)) };
           { vb_pat = (Pat_var "z"); vb_expr = (Expr_const (Const_int 7)) }],
         (Expr_binop (Add,
            (Expr_binop (Add, (Expr_ident "x"), (Expr_ident "y"))),
            (Expr_ident "z")))
         )))
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

  $ ../bin/REPL.exe -dparsetree <<EOF
  > x y z w
  [(Str_eval
      (Expr_apply (
         (Expr_apply ((Expr_apply ((Expr_ident "x"), (Expr_ident "y"))),
            (Expr_ident "z"))),
         (Expr_ident "w"))))
    ]

  $ ../bin/REPL.exe -dparsetree <<EOF
  > fun x y z -> w
  [(Str_eval
      (Expr_fun ((Pat_var "x"),
         (Expr_fun ((Pat_var "y"), (Expr_fun ((Pat_var "z"), (Expr_ident "w")))
            ))
         )))
    ]

  $ ../bin/REPL.exe -dparsetree <<EOF
  > fun x y z -> x y z 
  [(Str_eval
      (Expr_fun ((Pat_var "x"),
         (Expr_fun ((Pat_var "y"),
            (Expr_fun ((Pat_var "z"),
               (Expr_apply ((Expr_apply ((Expr_ident "x"), (Expr_ident "y"))),
                  (Expr_ident "z")))
               ))
            ))
         )))
    ]

  $ ../bin/REPL.exe -dparsetree <<EOF
  > fun x y -> (fun z -> x y z)
  [(Str_eval
      (Expr_fun ((Pat_var "x"),
         (Expr_fun ((Pat_var "y"),
            (Expr_fun ((Pat_var "z"),
               (Expr_apply ((Expr_apply ((Expr_ident "x"), (Expr_ident "y"))),
                  (Expr_ident "z")))
               ))
            ))
         )))
    ]

  $ ../bin/REPL.exe -dparsetree <<EOF
  > x + y + z - w
  [(Str_eval
      (Expr_binop (Sub,
         (Expr_binop (Add,
            (Expr_binop (Add, (Expr_ident "x"), (Expr_ident "y"))),
            (Expr_ident "z"))),
         (Expr_ident "w"))))
    ]

  $ ../bin/REPL.exe -dparsetree <<EOF
  > x + y * w / z
  [(Str_eval
      (Expr_binop (Add, (Expr_ident "x"),
         (Expr_binop (Div,
            (Expr_binop (Mult, (Expr_ident "y"), (Expr_ident "w"))),
            (Expr_ident "z")))
         )))
    ]

  $ ../bin/REPL.exe -dparsetree <<EOF
  > x + y > x - y
  [(Str_eval
      (Expr_binop (Gt, (Expr_binop (Add, (Expr_ident "x"), (Expr_ident "y"))),
         (Expr_binop (Sub, (Expr_ident "x"), (Expr_ident "y"))))))
    ]

  $ ../bin/REPL.exe -dparsetree <<EOF
  > x = y
  [(Str_eval (Expr_binop (Eq, (Expr_ident "x"), (Expr_ident "y"))))]

  $ ../bin/REPL.exe -dparsetree <<EOF
  > not true
  [(Str_eval (Expr_unop (Not, (Expr_const (Const_bool true)))))]

  $ ../bin/REPL.exe -dparsetree <<EOF
  > match mnc with | _ -> (hwumibc : 'ftxcdkouhyyumgetu) | 310 -> ()
  [(Str_eval
      (Expr_match ((Expr_ident "mnc"),
         { case_pat = Pat_any;
           case_expr =
           (Expr_constraint ((Type_var "ftxcdkouhyyumgetu"),
              (Expr_ident "hwumibc")))
           },
         [{ case_pat = (Pat_constant (Const_int 310));
            case_expr = (Expr_const Const_unit) }
           ]
         )))
    ]

  $ ../bin/REPL.exe -dparsetree <<EOF
  > let rec fix f x = f (fix f) x
  [(Str_value (Recursive,
      { vb_pat = (Pat_var "fix");
        vb_expr =
        (Expr_fun ((Pat_var "f"),
           (Expr_fun ((Pat_var "x"),
              (Expr_apply (
                 (Expr_apply ((Expr_ident "f"),
                    (Expr_apply ((Expr_ident "fix"), (Expr_ident "f"))))),
                 (Expr_ident "x")))
              ))
           ))
        },
      []))
    ]

  $ ../bin/REPL.exe -dparsetree <<EOF
  > let rec fact n = if n = 1 then 1 else n * fact (n - 1);;
  [(Str_value (Recursive,
      { vb_pat = (Pat_var "fact");
        vb_expr =
        (Expr_fun ((Pat_var "n"),
           (Expr_if (
              (Expr_binop (Eq, (Expr_ident "n"), (Expr_const (Const_int 1)))),
              (Expr_const (Const_int 1)),
              (Some (Expr_binop (Mult, (Expr_ident "n"),
                       (Expr_apply ((Expr_ident "fact"),
                          (Expr_binop (Sub, (Expr_ident "n"),
                             (Expr_const (Const_int 1))))
                          ))
                       )))
              ))
           ))
        },
      []))
    ]

  $ ../bin/REPL.exe -dparsetree <<EOF
  > varKatya
  [(Str_eval (Expr_ident "varKatya"))]

  $ ../bin/REPL.exe -dparsetree <<EOF
  > ifx theny
  [(Str_eval (Expr_apply ((Expr_ident "ifx"), (Expr_ident "theny"))))]

  $ ../bin/REPL.exe -dparsetree <<EOF
  > Katya
  : end_of_input

  $ ../bin/REPL.exe -dparsetree <<EOF
  > let type = 123
  : end_of_input

  $ ../bin/REPL.exe -dparsetree <<EOF
  > let rec = true
  : end_of_input

  $ ../bin/REPL.exe -dparsetree <<EOF
  > katya_08Kat'ya
  [(Str_eval (Expr_ident "katya_08Kat'ya"))]

  $ ../bin/REPL.exe -dparsetree <<EOF
  > letrec x = x
  [(Str_eval
      (Expr_binop (Eq, (Expr_apply ((Expr_ident "letrec"), (Expr_ident "x"))),
         (Expr_ident "x"))))
    ]

  $ ../bin/REPL.exe -dparsetree <<EOF
  > let x = 123x
  [(Str_value (NonRecursive,
      { vb_pat = (Pat_var "x"); vb_expr = (Expr_const (Const_int 123)) }, 
      []));
    (Str_eval (Expr_ident "x"))]

  $ ../bin/REPL.exe -dparsetree <<EOF
  > let  _ = 8
  [(Str_value (NonRecursive,
      { vb_pat = Pat_any; vb_expr = (Expr_const (Const_int 8)) }, []))
    ]
