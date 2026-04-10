[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type error =
  | LimitError
  | ZeroDivision
  | InvalidApplication
  | InvalidLet
  | UnboundVariable of Ast.name
  | TypeMismatch of string
  | TypesMismatch of string * string

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

let rec show_value_type = function
  | Unit -> "unit"
  | Int _ -> "int"
  | Bool _ -> "bool"
  | Tuple (t1, t2, ts) -> String.concat " * " (List.map show_value_type (t1 :: t2 :: ts))
  | Closure _ -> "<closure>"
  | BuiltinAbstraction _ -> "<built-in>"
;;

let rec show_value = function
  | Unit -> "()"
  | Int int -> string_of_int int
  | Bool bool -> string_of_bool bool
  | Tuple (v1, v2, vs) -> String.concat " * " (List.map show_value (v1 :: v2 :: vs))
  | _ -> ""
;;

module Interpreter (ST : Utils.STATE_MONAD) = struct
  let return x = ST.return (Eval x)
  let fail e = ST.return (EvalError e)

  (* Two different bind operators here *)
  (* '>>=' is used for State *)
  (* 'let*' is used for Eval *)

  let ( >>= ) = ST.bind

  let ( let* ) x transform =
    ST.bind x (function
      | Eval x -> transform x
      | EvalError e -> ST.return (EvalError e))
  ;;

  let get_vars = ST.bind ST.read (fun env -> ST.return (fst env))
  let get_limit = ST.bind ST.read (fun env -> ST.return (snd env))

  let update_vars var value =
    ST.bind ST.read (fun (vars, limit) -> ST.write ((var, value) :: vars, limit))
  ;;

  let set_vars vars = ST.bind ST.read (fun env -> ST.write (vars, snd env))

  let update_limit =
    ST.bind ST.read (fun st ->
      match st with
      | _, Unlimited -> ST.return Unlimited
      | env, Limited n when n > 0 ->
        ST.write (env, Limited (n - 1)) >>= fun _ -> ST.return (Limited (n - 1))
      | _, Limited n -> ST.return (Limited n))
  ;;

  let eval_unop operator expr =
    match operator, expr with
    | Ast.Neg, Int e -> return (Int (-e))
    | Ast.Neg, _ -> fail (TypeMismatch (show_value_type expr))
    | Ast.Not, Bool e -> return (Bool (not e))
    | Ast.Not, _ -> fail (TypeMismatch (show_value_type expr))
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
    | _, _, _ -> fail (TypesMismatch (show_value_type lhs, show_value_type rhs))
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
    update_limit
    >>= fun limit ->
    (match limit with
     | Limited 0 -> fail LimitError
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
       | _ -> fail (TypeMismatch (show_value_type vcond)))
    | Ast.LetExpr (Ast.Let, name, e1, e2) ->
      let* v1 = eval e1 in
      get_vars
      >>= fun saved_vars ->
      update_vars name v1
      >>= fun _ ->
      let* v2 = eval e2 in
      get_vars >>= fun _ -> set_vars saved_vars >>= fun _ -> return v2
    | Ast.LetExpr (Ast.LetRec, name, e1, e2) ->
      (match e1 with
       | Ast.Abstraction _ ->
         get_limit
         >>= fun limit ->
         get_vars
         >>= fun saved_vars ->
         let rec v = Closure (e1, ((name, v) :: saved_vars, limit)) in
         update_vars name v
         >>= fun _ ->
         let* v2 = eval e2 in
         get_vars >>= fun _ -> set_vars saved_vars >>= fun _ -> return v2
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
         >>= fun saved_vars ->
         set_vars ((arg, v2) :: fst env)
         >>= fun _ ->
         let* result = eval expr in
         get_vars >>= fun _ -> set_vars saved_vars >>= fun _ -> return result
       | BuiltinAbstraction f ->
         (match f v2 with
          | Eval v -> return v
          | EvalError e -> fail e)
       | _ -> fail InvalidApplication)
  ;;
end

let initial_env ?(steps = Unlimited) =
  let print =
    BuiltinAbstraction
      (function
        | Unit ->
          print_string "()";
          Eval Unit
        | Int int ->
          print_int int;
          Eval Unit
        | Bool bool ->
          print_string (string_of_bool bool);
          Eval Unit
        | _ -> EvalError InvalidApplication)
  in
  let first =
    BuiltinAbstraction
      (function
        | Tuple (v1, _, _) -> Eval v1
        | _ -> EvalError InvalidApplication)
  in
  let second =
    BuiltinAbstraction
      (function
        | Tuple (_, v1, vs) when vs = [] -> Eval v1
        | Tuple (_, v1, v2 :: vs) -> Eval (Tuple (v1, v2, vs))
        | _ -> EvalError InvalidApplication)
  in
  [ "print", print; "fst", first; "snd", second ], steps
;;

let run env expr =
  let module E = Interpreter (State) in
  State.run (E.eval expr) env |> snd
;;
