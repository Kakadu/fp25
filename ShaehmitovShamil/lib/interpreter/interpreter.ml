open Ast
open Base

module type Eval = sig
  type value
  type error
  type env
  type 'a eval_result = ('a, error) result

  val show_error : error -> string
  val run_program : int -> program -> (env * value option, error) result
end

module Interpreter : Eval = struct
  type error =
    | UnboundVariable of name
    | TypeError of string
    | DivisionByZero
    | OutOfMaxSteps

  type 'a eval_result = ('a, error) result
  type is_rec = bool

  type value =
    | VInt of int
    | VUnit
    | VClosure of is_rec * name * env * pattern list * expr
    | VBuiltin of (value -> value eval_result)

  and env = (name, value, String.comparator_witness) Map.t

  let ( let* ) res f =
    match res with
    | Ok v -> f v
    | Error e -> Error e
  ;;

  let return v = Ok v

  let lookup_env (env : env) (x : name) : value eval_result =
    match Map.find env x with
    | Some v -> return v
    | None -> Error (UnboundVariable x)
  ;;

  let extend env key value = Map.update env key ~f:(fun _ -> value)

  let rec eval_expr (env : env) (e : expr) (steps : int) : (value * int) eval_result =
    if steps <= 0
    then Error OutOfMaxSteps
    else (
      let steps = steps - 1 in
      match e with
      | Const (CInt i) -> return (VInt i, steps)
      | Const CUnit -> return (VUnit, steps)
      | Var name ->
        let* v = lookup_env env name in
        return (v, steps)
      | UnOp (Neg, e) ->
        let* v, steps = eval_expr env e steps in
        (match v with
         | VInt i -> return (VInt (-i), steps)
         | _ -> Error (TypeError "Expected integer for negation"))
      | BinOp (op, e1, e2) ->
        let* v1, steps = eval_expr env e1 steps in
        let* v2, steps = eval_expr env e2 steps in
        (match v1, v2 with
         | VInt i1, VInt i2 ->
           (match op with
            | Add -> return (VInt (i1 + i2), steps)
            | Sub -> return (VInt (i1 - i2), steps)
            | Mul -> return (VInt (i1 * i2), steps)
            | Div ->
              if i2 = 0 then Error DivisionByZero else return (VInt (i1 / i2), steps)
            | Eq -> return (VInt (if i1 = i2 then 1 else 0), steps)
            | Neq -> return (VInt (if i1 <> i2 then 1 else 0), steps)
            | Lt -> return (VInt (if i1 < i2 then 1 else 0), steps)
            | Le -> return (VInt (if i1 <= i2 then 1 else 0), steps)
            | Gt -> return (VInt (if i1 > i2 then 1 else 0), steps)
            | Ge -> return (VInt (if i1 >= i2 then 1 else 0), steps))
         | _ -> Error (TypeError "Cannot apply binary operator to non-integers"))
      | If (cond, t, f) ->
        let* v, steps = eval_expr env cond steps in
        (match v with
         | VInt 0 -> eval_expr env f steps
         | VInt _ -> eval_expr env t steps
         | _ -> Error (TypeError "Expected integer in if condition"))
      | FunExpr (params, body) -> return (VClosure (false, "", env, params, body), steps)
      | Let (NonRec, p, e1, e2) ->
        let* v1, steps = eval_expr env e1 steps in
        (match p with
         | PVar name ->
           let new_env = extend env name v1 in
           eval_expr new_env e2 steps
         | PAny -> eval_expr env e2 steps
         | PUnit ->
           (match v1 with
            | VUnit -> eval_expr env e2 steps
            | _ -> Error (TypeError "Expected unit value for unit pattern")))
      | Let (Rec, p, e1, e2) ->
        (match p, e1 with
         | PVar name, FunExpr (params, body) ->
           let rec_closure = VClosure (true, name, env, params, body) in
           let new_env = extend env name rec_closure in
           eval_expr new_env e2 steps
         | PVar _, _ -> Error (TypeError "Recursive let binding must be a function")
         | PAny, _ -> Error (TypeError "Recursive let binding cannot be a wildcard")
         | PUnit, _ -> Error (TypeError "Recursive let binding cannot be unit"))
      | App (f, arg) ->
        let* func, steps = eval_expr env f steps in
        let* arg_val, steps = eval_expr env arg steps in
        (match func with
         | VClosure (false, name, closure_env, params, body) ->
           (match params with
            | [] -> Error (TypeError "Applying argument to a function with no parameters")
            | p :: rest ->
              let* new_env =
                match p with
                | PVar name -> return (extend closure_env name arg_val)
                | PAny -> return closure_env
                | PUnit ->
                  (match arg_val with
                   | VUnit -> return closure_env
                   | _ -> Error (TypeError "Expected unit value for unit pattern"))
              in
              if List.length rest = 0
              then eval_expr new_env body steps
              else return (VClosure (false, name, new_env, rest, body), steps))
         | VClosure (true, name, closure_env, params, body) ->
           let recursive_env = extend closure_env name func in
           (match params with
            | [] -> Error (TypeError "Applying argument to a function with no parameters")
            | p :: rest ->
              let* new_env =
                match p with
                | PVar name -> return (extend recursive_env name arg_val)
                | PAny -> return recursive_env
                | PUnit ->
                  (match arg_val with
                   | VUnit -> return recursive_env
                   | _ -> Error (TypeError "Expected unit value for unit pattern"))
              in
              if List.length rest = 0
              then eval_expr new_env body steps
              else return (VClosure (false, name, new_env, rest, body), steps))
         | VBuiltin builtin_fn ->
           let* res = builtin_fn arg_val in
           return (res, steps)
         | _ -> Error (TypeError "Cannot apply a non-function")))
  ;;

  let eval_program_item (env : env) (item : structure_item) (steps : int)
    : ((env * value option) * int) eval_result
    =
    match item with
    | Value (NonRec, p, expr) ->
      let* v, steps = eval_expr env expr steps in
      (match p with
       | PVar name -> return ((extend env name v, None), steps)
       | PAny -> return ((env, None), steps)
       | PUnit ->
         (match v with
          | VUnit -> return ((env, None), steps)
          | _ -> Error (TypeError "Expected unit value for unit pattern")))
    | Value (Rec, p, expr) ->
      (match p, expr with
       | PVar name, FunExpr (params, body) ->
         let rec_closure = VClosure (true, name, env, params, body) in
         let new_env = extend env name rec_closure in
         return ((new_env, None), steps)
       | PVar _, _ -> Error (TypeError "Recursive value definition must be a function")
       | PAny, _ -> Error (TypeError "Recursive value definition cannot be a wildcard")
       | PUnit, _ -> Error (TypeError "Recursive value definition cannot be unit"))
  ;;

  let show_error = function
    | UnboundVariable name -> "Error: Unbound variable " ^ name
    | TypeError msg -> "Error: Type error - " ^ msg
    | DivisionByZero -> "Error: Division by zero"
    | OutOfMaxSteps -> "Error: Out of maximum allowed evaluation steps"
  ;;

  let initial_env : env =
    let open Base.Map in
    empty (module String)
    |> set
         ~key:"print_int"
         ~data:
           (VBuiltin
              (function
                | VInt i ->
                  print_int i;
                  return VUnit
                | _ -> Error (TypeError "print_int expects an integer")))
    |> set
         ~key:"print_endl"
         ~data:
           (VBuiltin
              (function
                | VUnit ->
                  print_newline ();
                  return VUnit
                | _ -> Error (TypeError "print_endl expects a unit value ()")))
    |> set
         ~key:"fix"
         ~data:
           (VBuiltin
              (function
                | VClosure (_, _, closure_env, params, body) ->
                  (match params with
                   | PVar name :: rest ->
                     return (VClosure (true, name, closure_env, rest, body))
                   | _ :: _ ->
                     Error (TypeError "First argument of fix must be a named variable")
                   | [] ->
                     Error
                       (TypeError "Function passed to fix must have at least one argument"))
                | _ -> Error (TypeError "fix expects a function")))
  ;;

  let run_program (max_steps : int) (prog : program) =
    let* final_env, final_val, _ =
      List.fold_left
        prog
        ~init:(return (initial_env, None, max_steps))
        ~f:(fun acc item ->
          let* env, _, steps = acc in
          let* (new_env, v_opt), steps' = eval_program_item env item steps in
          return (new_env, v_opt, steps'))
    in
    return (final_env, final_val)
  ;;
end
