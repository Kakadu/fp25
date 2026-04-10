[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type error =
  | ExhaustedSteps
  | ZeroDivision
  | InvalidApplication
  | InvalidLet
  | UnboundVariable of Ast.name
  | TypeMismatch of string * string

type 'a result =
  | Eval of 'a
  | EvalError of error

type step_limit =
  | Unlimited
  | Limited of int

type env = (Ast.name * value) list * step_limit
and variables = (Ast.name * value) list

and value =
  | Unit
  | Int of int
  | Bool of bool
  | Tuple of value * value * value list
  | Closure of Ast.t * env
  | BuiltinAbstraction of (value -> value result)

module State : Utils.STATE_MONAD = struct
  type ('s, 'a) t = 's -> 's * 'a

  let return x st = st, x

  let bind x transform state =
    let st, x = x state in
    transform x st
  ;;

  let read : ('st, 'st) t = fun state -> state, state
  let write : 'st -> ('st, unit) t = fun s _ -> s, ()
  let run func state = func state
end

let show_value_type = function
  | Unit -> "unit"
  | Int _ -> "int"
  | Bool _ -> "bool"
  | Tuple _ -> ""
  | Closure _ -> "<closure>"
  | BuiltinAbstraction _ -> "<built-in>"
;;

let show_value = function
  | Unit -> "()"
  | Int int -> string_of_int int
  | Bool bool -> string_of_bool bool
  | _ -> ""
;;

module Eval (M : Utils.STATE_MONAD) = struct
  let return x = M.return (Eval x)
  let fail e = M.return (EvalError e)

  let bind x transform =
    M.bind x (function
      | Eval x -> transform x
      | EvalError e -> M.return (EvalError e))
  ;;

  let get_vars = M.bind M.read (fun env -> M.return (fst env))
  let set_vars vars = M.bind M.read (fun env -> M.write (vars, snd env))
  let get_limit = M.bind M.read (fun env -> M.return (snd env))

  let decrease_limit =
    M.bind M.read (fun st ->
      match st with
      | _, Unlimited -> M.return ()
      | env, Limited n when n > 0 -> M.write (env, Limited (n - 1))
      | _ -> M.return ())
  ;;

  let ( >>= ) = M.bind
  let ( let* ) = bind

  let eval_unop operator expr =
    match operator, expr with
    | Ast.Neg, Int e -> return (Int (-e))
    | Ast.Neg, _ -> fail (TypeMismatch ("int", show_value_type expr))
    | Ast.Not, Bool e -> return (Bool (not e))
    | Ast.Not, _ -> fail (TypeMismatch ("bool", show_value_type expr))
  ;;

  let eval_binop operator lhs rhs =
    match operator, lhs, rhs with
    | Ast.Add, Int l, Int r -> return (Int (l + r))
    | Ast.Sub, Int l, Int r -> return (Int (l - r))
    | Ast.Mul, Int l, Int r -> return (Int (l * r))
    | Ast.Div, Int _, Int r when r = 0 -> fail ZeroDivision
    | Ast.Div, Int l, Int r -> return (Int (l / r))
    | Ast.Mod, Int _, Int r when r = 0 -> fail ZeroDivision
    | Ast.Mod, Int l, Int r -> return (Int (l mod r))
    | Ast.And, Bool l, Bool r -> return (Bool (l && r))
    | Ast.Or, Bool l, Bool r -> return (Bool (l || r))
    | Ast.Equal, Int l, Int r -> return (Bool (l = r))
    | Ast.Equal, Bool l, Bool r -> return (Bool (l = r))
    | Ast.NotEqual, Int l, Int r -> return (Bool (l <> r))
    | Ast.NotEqual, Bool l, Bool r -> return (Bool (l <> r))
    | Ast.Less, Int l, Int r -> return (Bool (l < r))
    | Ast.LessEqual, Int l, Int r -> return (Bool (l <= r))
    | Ast.Greater, Int l, Int r -> return (Bool (l > r))
    | Ast.GreaterEqual, Int l, Int r -> return (Bool (l >= r))
    | _, _, _ -> fail (TypeMismatch (show_value_type lhs, show_value_type rhs))
  ;;

  let rec lookup name vars =
    match name with
    | Ast.Wildcard -> None
    | Ast.Real name_str ->
      (match vars with
       | [] -> None
       | (Ast.Real str, value) :: _ when str = name_str -> Some value
       | _ :: tl -> lookup name tl)
  ;;

  let rec eval expr =
    decrease_limit
    >>= fun _ ->
    get_limit
    >>= fun limit ->
    (match limit with
     | Limited 0 -> fail ExhaustedSteps
     | _ -> return ())
    >>= fun _ ->
    match expr with
    | Ast.Unit -> return Unit
    | Ast.Int int -> return (Int int)
    | Ast.Bool bool -> return (Bool bool)
    | Ast.Var name ->
      get_vars
      >>= fun vars ->
      (match lookup name vars with
       | None -> fail (UnboundVariable name)
       | Some v -> return v)
    | Ast.Tuple (fst, snd, tl) ->
      let* v1 = eval fst in
      let* v2 = eval snd in
      let rec eval_list acc = function
        | [] -> return (List.rev acc)
        | e :: rest ->
          let* v = eval e in
          eval_list (v :: acc) rest
      in
      let* vs = eval_list [] tl in
      return (Tuple (v1, v2, vs))
    | Ast.UnaryOp (op, expr) ->
      let* v = eval expr in
      eval_unop op v
    | Ast.BinaryOp (op, left, right) ->
      let* v1 = eval left in
      let* v2 = eval right in
      eval_binop op v1 v2
    | Ast.IfThenElse (cond, e1, e2) ->
      let* vcond = eval cond in
      (match vcond with
       | Bool true -> eval e1
       | Bool false -> eval e2
       | _ -> fail (TypeMismatch ("bool", show_value_type vcond)))
    | Ast.LetExpr (Ast.Let, name, e1, e2) ->
      let* v1 = eval e1 in
      get_vars
      >>= fun vars ->
      set_vars ((name, v1) :: vars)
      >>= fun _ ->
      let* v2 = eval e2 in
      get_vars >>= fun _ -> set_vars vars >>= fun _ -> return v2
    | Ast.LetExpr (Ast.LetRec, name, e1, e2) ->
      (match e1 with
       | Ast.Abstraction _ ->
         get_limit
         >>= fun limit ->
         get_vars
         >>= fun vars ->
         let rec v = Closure (e1, ((name, v) :: vars, limit)) in
         set_vars ((name, v) :: vars)
         >>= fun () ->
         let* v2 = eval e2 in
         get_vars >>= fun _ -> set_vars vars >>= fun () -> return v2
       | _ -> fail InvalidLet)
    | Ast.Abstraction _ as abs ->
      get_limit
      >>= fun limit -> get_vars >>= fun vars -> return (Closure (abs, (vars, limit)))
    | Ast.Application (expr1, expr2) ->
      let* v1 = eval expr1 in
      let* v2 = eval expr2 in
      (match v1 with
       | Closure (Ast.Abstraction (arg, expr), env) ->
         get_vars
         >>= fun saved ->
         set_vars ((arg, v2) :: fst env)
         >>= fun _ ->
         let* result = eval expr in
         get_vars >>= fun _ -> set_vars saved >>= fun _ -> return result
       | BuiltinAbstraction f ->
         (match f v2 with
          | Eval v -> return v
          | EvalError e -> fail e)
       | _ -> fail InvalidApplication)
  ;;
end
