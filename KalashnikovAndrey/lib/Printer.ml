open Ast

let op_to_string = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Lt -> "<"
  | Eq -> "="
  | Mt -> ">"

let rec pp fmt = function
  | Const n ->
      Format.fprintf fmt "%d" n
  | Var x ->
      Format.fprintf fmt "%s" x
  | Neg e ->
      Format.fprintf fmt "(-%a)" pp e
  | BinOp (op, left, right) ->
      Format.fprintf fmt "(%a %s %a)" pp left (op_to_string op) pp right
  | If (cond, t, e) ->
      Format.fprintf fmt "(if %a then %a else %a)" pp cond pp t pp e
  | App (func, arg) ->
      Format.fprintf fmt "(%a %a)" pp func pp arg
  | Fun (arg_name, body) ->
      Format.fprintf fmt "(fun %s -> %a)" arg_name pp body
  | Fix e ->
      Format.fprintf fmt "(fix (%a))" pp e
  | Let (rec_flag, name, e1, e2) ->
      let rec_str =
        match rec_flag with
        | Rec -> "rec "
        | Val -> ""
      in
      Format.fprintf fmt "(let %s%s = %a in %a)" rec_str name pp e1 pp e2
