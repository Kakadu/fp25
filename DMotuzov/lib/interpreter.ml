open Ast

type error =
  | TypeError
  | DivisionByZero
  | NoVariable of identificator

type value =
  | ValInt of int
  | ValUnit
  | ValFun of identificator * expression * env

and env = (identificator, value, Base.String.comparator_witness) Base.Map.t

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

module EvalEnv = struct
  open Base

  let empty = Map.empty (module String)
  let extend env key value = Map.update env key ~f:(fun _ -> value)

  let compose env1 env2 =
    Map.fold env2 ~f:(fun ~key ~data env_acc -> extend env_acc key data) ~init:env1
  ;;

  let find_exn env key =
    match Map.find env key with
    | Some value -> Res.return value
    | None -> Res.fail (NoVariable key)
  ;;
end

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

  let rec eval_expression env = function
    | Expr_var id -> find_exn env id
    | Expr_const const ->
      (match const with
       | Const_int int -> return (ValInt int)
       | Const_unit -> return ValUnit)
    | Expr_let_in (id, expr1, expr2) ->
      let* value = eval_expression env expr1 in
      let env' = EvalEnv.extend env id value in
      eval_expression env' expr2
    | Expr_binary_op (bin_op, expr1, expr2) ->
      let* value1 = eval_expression env expr1 in
      let* value2 = eval_expression env expr2 in
      eval_bin_op (bin_op, value1, value2)
    | Expr_conditional (expr1, expr2, expr3) ->
      let* value1 = eval_expression env expr1 in
      (match value1 with
       | ValInt n ->
         if n <> 0 then eval_expression env expr2 else eval_expression env expr3
       | _ -> fail TypeError)
    | _ -> fail TypeError
  ;;

  let eval_top_let env = function
    | Top_let (id, expr) ->
      let* value = eval_expression env expr in
      let env' = EvalEnv.extend env id value in
      return env'
    | _ -> fail TypeError
  ;;

  let rec eval_program env = function
    | [] -> Res.return env
    | top :: rest ->
      let* env' = eval_top_let env top in
      eval_program env' rest
  ;;
end

let run_interpreter program = Inter.eval_program EvalEnv.empty program
