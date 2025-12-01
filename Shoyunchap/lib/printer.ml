open Ast

(** constants **)
let string_of_const = function
  | Int n  -> string_of_int n
  | Unit   -> "()"

(** binary operations **)
let string_of_op = function
  | OpAdd -> "+"
  | OpSub -> "-"
  | OpMul -> "*"
  | OpDiv -> "/"
  | OpEq  -> "="
  | OpGt  -> ">"
  | OpLt  -> "<"
  | OpGte -> ">="
  | OpLte -> "<="

(** helper: from Fun x -> Fun y -> ... to list of args + body **)
let rec collect_fun args = function
  | Fun (x, body) -> collect_fun (x :: args) body
  | e             -> List.rev args, e

(* Нужна, чтобы расставлять скобки только где нужно *)
let is_atomic = function
  | Const _ | Var _ -> true
  | _ -> false

let rec string_of_expr (e : expression) : string =
  match e with
  | Const c ->
      string_of_const c

  | Var x ->
      x

  | Fun (x, body) ->
      let args, core = collect_fun [x] body in
      let args_s = String.concat " " args in
      Printf.sprintf "fun %s -> %s" args_s (string_of_expr core)

  | App (f, arg) ->
      (* собираем цепочку f a b c, чтобы не городить лишних скобок *)
      let rec gather acc = function
        | App (f', a') -> gather (a' :: acc) f'
        | other        -> other, acc
      in
      let f0, args = gather [arg] f in
      let parts =
        string_of_app_head f0
        :: List.map string_of_app_arg args
      in
      String.concat " " parts

  | BinOp (op, l, r) ->
      (* для простоты всегда берём скобки вокруг бинарного выражения *)
      Printf.sprintf "(%s %s %s)"
        (string_of_expr l)
        (string_of_op op)
        (string_of_expr r)

  | If (cond, thn, els_opt) ->
      let base =
        Printf.sprintf "if %s then %s"
          (string_of_expr cond)
          (string_of_expr thn)
      in
      begin match els_opt with
      | None      -> base
      | Some els  -> base ^ " else " ^ string_of_expr els
      end

  | Let (scope, kind, name, value, body_opt) ->
      let scope_prefix =
        match scope with
        | GlobalVar -> ""   (* можно было бы помечать, но обычно не печатают *)
        | LocalVar  -> ""
      in
      let rec_kwd =
        match kind with
        | NonRec -> ""
        | Rec    -> " rec"
      in
      let binding =
        Printf.sprintf "%slet%s %s = %s"
          scope_prefix
          rec_kwd
          name
          (string_of_expr value)
      in
      begin match body_opt with
      | None ->
          binding
      | Some body ->
          Printf.sprintf "%s in %s" binding (string_of_expr body)
      end

(* Вспомогалки для аппликации: голова и аргументы *)

and string_of_app_head e =
  (* Голова приложения: если это не атом, оборачиваем в скобки *)
  if is_atomic e then
    string_of_expr e
  else
    "(" ^ string_of_expr e ^ ")"

and string_of_app_arg e =
  (* Аргументы тоже лучше иногда скобочить, чтобы избежать двусмысленности *)
  match e with
  | Const _ | Var _ -> string_of_expr e
  | _               -> "(" ^ string_of_expr e ^ ")"
