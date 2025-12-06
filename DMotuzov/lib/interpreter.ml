open Ast

type error =
  | TypeError
  | DivisionByZero
  | NoVariable of identificator
  | OutOfMaxSteps

module Res = struct
  open Base

  type 'a t = ('a, error) Result.t

  let fail = Result.fail
  let return = Result.return

  let ( >>= ) (monad : 'a t) (f : 'a -> 'b t) : 'b t =
    match monad with
    | Ok result -> f result
    | Error x -> fail x
  ;;

  let ( let* ) = ( >>= )
end

type value =
  | ValInt of int
  | ValUnit
  | ValFun of identificator * expression * env
  | RecClosure of identificator * identificator * expression * env
  | Builtin of (value -> value Res.t)

and env = (identificator, value, Base.String.comparator_witness) Base.Map.t

module EvalEnv = struct
  open Base

  let empty = Map.empty (module String)
  let extend env key value = Map.update env key ~f:(fun _ -> value)

  let find_expr env key =
    match Map.find env key with
    | Some value -> Res.return value
    | None -> Res.fail (NoVariable key)
  ;;
end

let empty_with_builtins =
  EvalEnv.extend
    EvalEnv.empty
    "print_int"
    (Builtin
       (function
         | ValInt n ->
           print_int n;
           print_newline ();
           Res.return ValUnit
         | _ -> Res.fail TypeError))
;;

module Inter = struct
  open Res
  open EvalEnv

  let eval_arith opr val1 val2 = return (ValInt (opr val1 val2))

  let eval_bin_op = function
    | Mul, ValInt val1, ValInt val2 -> eval_arith ( * ) val1 val2
    | Div, ValInt val1, ValInt val2 when val2 <> 0 -> eval_arith ( / ) val1 val2
    | Div, _, ValInt 0 -> fail DivisionByZero
    | Plus, ValInt val1, ValInt val2 -> eval_arith ( + ) val1 val2
    | Sub, ValInt val1, ValInt val2 -> eval_arith ( - ) val1 val2
    | _ -> fail TypeError
  ;;

  let rec eval_expression env expr steps =
    if steps <= 0
    then Res.fail OutOfMaxSteps
    else (
      let steps = steps - 1 in
      match expr with
      | Expr_fun (param, body) -> return (ValFun (param, body, env), steps)
      | Expr_ap (fun_expr, args) ->
        let* f_val, steps = eval_expression env fun_expr steps in
        let rec apply f_val args steps =
          match args with
          | [] -> return (f_val, steps)
          | arg :: rest ->
            let* arg_val, steps = eval_expression env arg steps in
            let* f_val =
              match f_val with
              | ValFun _ -> return f_val
              | RecClosure (id, param, body, closure_env) ->
                return (ValFun (param, body, EvalEnv.extend closure_env id f_val))
              | Builtin f ->
                let* _ = f arg_val in
                return f_val
              | _ -> fail TypeError
            in
            (match f_val with
             | ValFun (param, body, closure_env) ->
               let call_env = EvalEnv.extend closure_env param arg_val in
               let* res, steps = eval_expression call_env body steps in
               apply res rest steps
             | Builtin _ -> apply f_val rest steps
             | _ -> fail TypeError)
        in
        apply f_val args steps
      | Expr_let_rec_in (id, Expr_fun (param, body), expr2) ->
        let closure_value = RecClosure (id, param, body, env) in
        let env' = EvalEnv.extend env id closure_value in
        eval_expression env' expr2 steps
      | Expr_fix expr ->
        let* f_val, steps = eval_expression env expr steps in
        (match f_val with
         | ValFun (f, f_body, f_env) ->
           let* inner, steps = eval_expression f_env f_body steps in
           (match inner with
            | ValFun (x, body, body_env) ->
              let closure = RecClosure (f, x, body, body_env) in
              let closure_env = EvalEnv.extend body_env f closure in
              return (RecClosure (f, x, body, closure_env), steps)
            | _ -> fail TypeError)
         | _ -> fail TypeError)
      | Expr_var id ->
        let* expr = find_expr env id in
        return (expr, steps)
      | Expr_const const ->
        let const =
          match const with
          | Const_int int -> ValInt int
          | Const_unit -> ValUnit
        in
        return (const, steps)
      | Expr_let_in (id, expr1, expr2) ->
        let* value, steps = eval_expression env expr1 steps in
        let env' = EvalEnv.extend env id value in
        eval_expression env' expr2 steps
      | Expr_binary_op (bin_op, expr1, expr2) ->
        let* value1, steps = eval_expression env expr1 steps in
        let* value2, steps = eval_expression env expr2 steps in
        let* result = eval_bin_op (bin_op, value1, value2) in
        return (result, steps)
      | Expr_conditional (expr1, expr2, expr3) ->
        let* value1, steps = eval_expression env expr1 steps in
        (match value1 with
         | ValInt n ->
           if n <> 0
           then eval_expression env expr2 steps
           else eval_expression env expr3 steps
         | _ -> fail TypeError)
      | _ -> fail TypeError)
  ;;

  let eval_top_let env steps = function
    | Top_let (id, expr) ->
      let* value, steps = eval_expression env expr steps in
      let env' = EvalEnv.extend env id value in
      return (env', steps)
    | Top_let_rec (id, Expr_fun (param, body)) ->
      let closure_value = RecClosure (id, param, body, env) in
      let env' = EvalEnv.extend env id closure_value in
      return (env', steps)
    | _ -> fail TypeError
  ;;

  let rec eval_program env steps = function
    | [] -> Res.return (env, steps)
    | top :: rest ->
      let* env', steps = eval_top_let env steps top in
      eval_program env' steps rest
  ;;
end

let run_interpreter program maxsteps =
  match Inter.eval_program empty_with_builtins maxsteps program with
  | Ok (env, _) -> Res.return env
  | Error e -> Error e
;;
