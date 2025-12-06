open Ast

let pp =
  let rec pp_expr fmt expr =
    let pp_wrapped fmt e =
      match e with
      | Expr_var _ | Expr_const _ -> pp_expr fmt e
      | _ -> Format.fprintf fmt "(%a)" pp_expr e
    in
    match expr with
    | Expr_var id -> Format.fprintf fmt "%s" id
    | Expr_const (Const_int n) -> Format.fprintf fmt "%d" n
    | Expr_const Const_unit -> Format.fprintf fmt "()"
    | Expr_ap (f, args) ->
      Format.fprintf
        fmt
        "%a %a"
        pp_wrapped
        f
        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt " ") pp_wrapped)
        args
    | Expr_conditional (e1, e2, e3) ->
      Format.fprintf fmt "if %a then %a else %a" pp_expr e1 pp_expr e2 pp_expr e3
    | Expr_fun (arg, body) -> Format.fprintf fmt "fun %s -> %a" arg pp_expr body
    | Expr_binary_op (op, e1, e2) ->
      Format.fprintf
        fmt
        "(%a %s %a)"
        pp_wrapped
        e1
        (match op with
         | Plus -> "+"
         | Sub -> "-"
         | Mul -> "*"
         | Div -> "/")
        pp_wrapped
        e2
    | Expr_let_in (id, e1, e2) ->
      Format.fprintf fmt "let %s = %a in %a" id pp_expr e1 pp_expr e2
    | Expr_let_rec_in (id, e1, e2) ->
      Format.fprintf fmt "let rec %s = %a in %a" id pp_expr e1 pp_expr e2
    | Expr_fix e -> Format.fprintf fmt "fix (%a)" pp_expr e
  in
  pp_expr
;;

let pp_top_let fmt = function
  | Top_let (id, expr) -> Format.fprintf fmt "let %s = %a;;" id pp expr
  | Top_let_rec (id, expr) -> Format.fprintf fmt "let rec %s = %a;;" id pp expr
;;

let pp_prog fmt prog =
  Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "\n") pp_top_let fmt prog
;;
