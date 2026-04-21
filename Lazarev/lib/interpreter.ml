[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type error =
  | InvalidApplication
  | InvalidLet
  | UnboundVariable of Ast.name
  | TypeMismatch of string
  | TypesMismatch of string * string

type 'a result =
  | Eval of 'a
  | EvalRaise of string
  | EvalError of error

type step_limit =
  | Unlimited
  | Limited of int

type env = (Ast.name * value) list * step_limit

and value =
  | Unit
  | Int of int
  | Bool of bool
  | Tuple of value * value * value list
  | Exception of string
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
  let run func = func
end

let rec show_value_type = function
  | Unit -> "unit"
  | Int _ -> "int"
  | Bool _ -> "bool"
  | Tuple (t1, t2, ts) -> String.concat " * " (List.map show_value_type (t1 :: t2 :: ts))
  | Exception _ -> "exception"
  | Closure _ -> "<closure>"
  | BuiltinAbstraction _ -> "<built-in>"
;;

module Result (S : Utils.STATE_MONAD) = struct
  let return x = S.return (Eval x)
  let fail e = S.return (EvalError e)
  let raise e = S.return (EvalRaise e)

  (* Two different bind operators here *)
  (* '>>=' is used for State *)
  (* 'let*' is used for Eval *)

  let ( >>= ) = S.bind

  let ( let* ) x transform =
    S.bind x (function
      | Eval x -> transform x
      | EvalRaise e -> raise e
      | EvalError e -> fail e)
  ;;

  let get_vars = S.bind S.read (fun env -> S.return (fst env))
  let get_limit = S.bind S.read (fun env -> S.return (snd env))

  let update_vars var value =
    S.bind S.read (fun (vars, limit) -> S.write ((var, value) :: vars, limit))
  ;;

  let set_vars vars = S.bind S.read (fun env -> S.write (vars, snd env))
  let set_limit limit = S.bind S.read (fun env -> S.write (fst env, limit))

  let update_limit =
    S.bind S.read (function
      | _, Unlimited -> S.return ()
      | env, Limited n when n > 0 -> S.write (env, Limited (n - 1))
      | _, Limited _ -> S.return ())
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
    | Ast.Div, Int _, Int r when r = 0 -> raise "DivisionByZero"
    | Ast.Div, Int l, Int r -> return (Int (l / r))
    | Ast.Mod, Int _, Int r when r = 0 -> raise "DivisionByZero"
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
    | Ast.Real name' ->
      (match vars with
       | [] -> None
       | (Ast.Real str, value) :: _ when String.equal str name' -> Some value
       | _ :: tl -> lookup name tl)
  ;;

  let rec eval expr =
    let* _ =
      get_limit
      >>= function
      | Limited 0 -> raise "StepsOverflow"
      | _ -> return ()
    in
    let* _ = update_limit >>= fun _ -> return () in
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
    | Ast.Tuple (v1, v2, vs) ->
      let* v1 = eval v1 in
      let* v2 = eval v2 in
      let rec eval_list acc = function
        | [] -> return (List.rev acc)
        | e :: rest ->
          let* v = eval e in
          eval_list (v :: acc) rest
      in
      let* vs = eval_list [] vs in
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
         let rec self = Closure (e1, ((name, self) :: saved_vars, limit)) in
         update_vars name self
         >>= fun _ ->
         let* v2 = eval e2 in
         set_vars saved_vars >>= fun _ -> return v2
       | _ -> fail InvalidLet)
    | Ast.Abstraction _ as abs ->
      get_limit
      >>= fun limit -> get_vars >>= fun vars -> return (Closure (abs, (vars, limit)))
    | Ast.Application (e1, e2) ->
      let* v1 = eval e1 in
      let* v2 = eval e2 in
      (match v1 with
       | Closure (Ast.Abstraction (arg, expr), env) ->
         get_vars
         >>= fun saved_vars ->
         set_vars ((arg, v2) :: fst env)
         >>= fun _ ->
         let* result = eval expr in
         set_vars saved_vars >>= fun _ -> return result
       | BuiltinAbstraction f ->
         (match f v2 with
          | Eval v -> return v
          | EvalRaise e -> raise e
          | EvalError e -> fail e)
       | _ -> fail InvalidApplication)
    | Ast.Exception name ->
      let v = Exception name in
      update_vars (Ast.Real name) v >>= fun _ -> return v
    | Ast.Raise expr ->
      get_vars
      >>= fun vars ->
      let* v = eval expr in
      (match v with
       | Exception name as e when lookup (Ast.Real name) vars = Some e -> raise name
       | _ -> fail (TypesMismatch ("exception", show_value_type v)))
    | Ast.TryWith (e1, name, e2) ->
      get_limit
      >>= fun saved_limit ->
      get_vars
      >>= fun saved_vars ->
      S.bind (eval e1) (function
        | Eval x -> return x
        | EvalRaise name' when String.equal name' name ->
          set_limit saved_limit
          >>= fun _ ->
          set_vars saved_vars
          >>= fun _ ->
          let* v2 = eval e2 in
          return v2
        | EvalRaise name -> raise name
        | EvalError error -> fail error)
  ;;
end

let get_builtins =
  let print_int =
    BuiltinAbstraction
      (function
        | Int int ->
          string_of_int int |> print_endline;
          Eval Unit
        | _ -> EvalError InvalidApplication)
  in
  let print_bool =
    BuiltinAbstraction
      (function
        | Bool bool ->
          string_of_bool bool |> print_endline;
          Eval Unit
        | _ -> EvalError InvalidApplication)
  in
  let get_first =
    BuiltinAbstraction
      (function
        | Tuple (v1, _, _) -> Eval v1
        | _ -> EvalError InvalidApplication)
  in
  let get_second =
    BuiltinAbstraction
      (function
        | Tuple (_, v1, vs) when vs = [] -> Eval v1
        | Tuple (_, v1, v2 :: vs) -> Eval (Tuple (v1, v2, vs))
        | _ -> EvalError InvalidApplication)
  in
  [ Ast.Real "print_int", print_int
  ; Ast.Real "print_bool", print_bool
  ; Ast.Real "fst", get_first
  ; Ast.Real "snd", get_second
  ]
;;

let new_env = get_builtins, Unlimited
let new_env_limited limit = get_builtins, Limited limit

let show_error = function
  | InvalidApplication -> "Invalid application"
  | InvalidLet -> "Invalid let-statement"
  | UnboundVariable name -> Format.sprintf "Unbound variable %S" (Ast.show_name name)
  | TypeMismatch typ -> Format.sprintf "Type mismatch: %S" typ
  | TypesMismatch (typ1, typ2) -> Format.sprintf "Types mismatch: %S and %S" typ1 typ2
;;

let show_result result =
  let rec show_eval = function
    | Unit -> "()"
    | Int int -> string_of_int int
    | Bool bool -> string_of_bool bool
    | Tuple (v1, v2, vs) -> String.concat ", " (List.map show_eval (v1 :: v2 :: vs))
    | Exception name -> name
    | Closure _ | BuiltinAbstraction _ -> "?"
  in
  match result with
  | Eval value -> Format.sprintf "%s: %s" (show_value_type value) (show_eval value)
  | EvalRaise exception_name -> Format.sprintf "Raised: %S" exception_name
  | EvalError error -> Format.sprintf "Error: %s" (show_error error)
;;

let run env expr =
  let module R = Result (State) in
  State.run (R.eval expr) env
;;
