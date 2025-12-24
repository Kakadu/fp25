open Ast

let string_of_unop = function
  | UMinus -> "-"
  | UPlus -> "+"
  | Not -> "not"
;;

let string_of_arith = function
  | Add -> "+"
  | Sub -> "-"
  | Div -> "/"
  | Mul -> "*"
;;

let string_of_cmp = function
  | Eq -> "="
  | Neq -> "<>"
  | Lt -> "<"
  | Le -> "<="
  | Gt -> ">"
  | Ge -> ">="
;;

let string_of_bool = function
  | And -> "&&"
  | Or -> "||"
;;

let rec collect_lams acc = function
  | Lam (x, body) -> collect_lams (x :: acc) body
  | body -> List.rev acc, body
;;

let pp_id fmt ident = Format.fprintf fmt "%s" ident

let pp_params fmt params =
  match params with
  | [] -> ()
  | _ ->
    Format.pp_print_list
      ~pp_sep:(fun fmt () -> Format.pp_print_string fmt " ")
      pp_id
      fmt
      params
;;

let rec pp_expr fmt = function
  | Const (Int n) -> Format.fprintf fmt "%d" n
  | Const (Bool true) -> Format.fprintf fmt "true"
  | Const (Bool false) -> Format.fprintf fmt "false"
  | Const (Unit ()) -> Format.fprintf fmt "()"
  | Var name -> Format.fprintf fmt "%s" name
  | Lam _ as expr ->
    let params, body = collect_lams [] expr in
    Format.fprintf fmt "(fun %a -> %a)" pp_params params pp_expr body
  | App (f, arg) -> Format.fprintf fmt "(%a %a)" pp_expr f pp_expr arg
  | Let (rf, name, rhs, body) ->
    let rec_kw =
      match rf with
      | Nonrec -> ""
      | Rec -> "rec "
    in
    Format.fprintf fmt "(let %s%s = %a in %a)" rec_kw name pp_expr rhs pp_expr body
  | If (cond, then_e, None) ->
    Format.fprintf fmt "(if %a then %a)" pp_expr cond pp_expr then_e
  | If (cond, then_e, Some else_e) ->
    Format.fprintf
      fmt
      "(if %a then %a else %a)"
      pp_expr
      cond
      pp_expr
      then_e
      pp_expr
      else_e
  | Unop (op, expr) -> Format.fprintf fmt "(%s %a)" (string_of_unop op) pp_expr expr
  | BinopArithmetic (op, left, right) ->
    Format.fprintf fmt "(%a %s %a)" pp_expr left (string_of_arith op) pp_expr right
  | BinopComp (op, left, right) ->
    Format.fprintf fmt "(%a %s %a)" pp_expr left (string_of_cmp op) pp_expr right
  | BinopBool (op, left, right) ->
    Format.fprintf fmt "(%a %s %a)" pp_expr left (string_of_bool op) pp_expr right
  | Fix expr -> Format.fprintf fmt "(fix %a)" pp_expr expr
;;

let pp_toplevel fmt = function
  | TExpr expr -> pp_expr fmt expr
  | TLet (rf, name, rhs) ->
    let rec_kw =
      match rf with
      | Nonrec -> ""
      | Rec -> "rec "
    in
    let params, body = collect_lams [] rhs in
    if params = []
    then Format.fprintf fmt "let %s%s = %a" rec_kw name pp_expr rhs
    else Format.fprintf fmt "let %s%s %a = %a" rec_kw name pp_params params pp_expr body
;;

let pp_program fmt program =
  let pp_item fmt item = Format.fprintf fmt "%a" pp_toplevel item in
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt ";;\n")
    pp_item
    fmt
    program
;;

let string_of_expr expr = Format.asprintf "%a" pp_expr expr
let string_of_toplevel item = Format.asprintf "%a" pp_toplevel item
let string_of_program program = Format.asprintf "%a" pp_program program
