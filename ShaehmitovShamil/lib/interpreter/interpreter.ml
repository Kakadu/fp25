open Ast
open Base

module type Eval = sig
  type value
  type error
  type 'a eval_result = ('a, error) result

  val show_value : value -> string
  val show_error : error -> string
  (* val run_program : program -> unit *)
end

module Interpreter : Eval = struct
  type error =
    | UnboundVariable of name
    | TypeError of string
    | DivisionByZero

  type 'a eval_result = ('a, error) result
  type is_rec = bool

  type value =
    | VInt of int
    | VUnit
    | VClosure of is_rec* name * env * pattern list * expr
    | VBuiltin of (value -> value eval_result)

  and env = (name, value, String.comparator_witness) Map.t

  let ( >>= ) res f =
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
  let rec eval_expr (env : env) (e : expr) : value eval_result =
    match e with
    | Const (CInt i) -> return (VInt i)
    | Var name -> lookup_env env name
    | UnOp (Neg, e) ->
      eval_expr env e
      >>= (function
       | VInt i -> return (VInt (-i))
       | _ -> Error (TypeError "Expected integer for negation"))
    | BinOp (op, e1, e2) ->
      eval_expr env e1
      >>= fun v1 ->
      eval_expr env e2
      >>= fun v2 ->
      (match v1, v2 with
       | VInt i1, VInt i2 ->
         (match op with
          | Add -> return (VInt (i1 + i2))
          | Sub -> return (VInt (i1 - i2))
          | Mul -> return (VInt (i1 * i2))
          | Div -> if i2 = 0 then Error DivisionByZero else return (VInt (i1 / i2)))
       | _ -> Error (TypeError "Cannot apply binary operator to non-integers"))
    | If (cond, t, f) ->
      eval_expr env cond
      >>= (function
       | VInt 0 -> eval_expr env f
       | VInt _ -> eval_expr env t
       | _ -> Error (TypeError "Expected integer in if condition"))
    | FunExpr (params, body) -> return (VClosure (false,"", env, params, body))
    | Let (NonRec, p, e1, e2) ->
      eval_expr env e1
      >>= fun v1 ->
      (match p with
       | PVar name ->
         let new_env = extend env name v1 in
         eval_expr new_env e2
       | PAny -> eval_expr env e2)
    | Let (Rec, p, e1, e2) ->
      (match p, e1 with
       | PVar name, FunExpr (params, body) ->
         let rec_closure = VClosure (true, name, env, params, body) in
         let new_env = extend env name rec_closure in
         eval_expr new_env e2
       | PVar _, _ -> Error (TypeError "Recursive let binding must be a function")
       | PAny, _ -> eval_expr env e1 >>= fun _ -> eval_expr env e2)
    | App (f, arg) ->
      eval_expr env f
      >>= fun func ->
      eval_expr env arg
      >>= fun arg_val ->
      (match func with
       | VClosure (false ,name, closure_env, params, body) ->
         (match params with
          | [] -> Error (TypeError "Applying argument to a function with no parameters")
          | p :: rest ->
            let new_env =
              match p with
              | PVar name -> extend closure_env name arg_val
              | PAny -> closure_env
            in
            if List.length rest = 0
            then eval_expr new_env body
            else return (VClosure (false, name, new_env, rest, body)))
      | VClosure (true , name, closure_env, params, body) ->
             let recursive_env = extend closure_env name func in
             (match params with
              | [] -> Error (TypeError "Applying argument to a function with no parameters")
              | p :: rest ->
                let new_env =
                  (match p with
                   | PVar name -> extend recursive_env name arg_val
                   | PAny -> recursive_env)
                in
                if List.length rest = 0
                then eval_expr new_env body
                else return (VClosure (false, name, new_env, rest, body)))
       | VBuiltin builtin_fn -> builtin_fn arg_val
       | _ -> Error (TypeError "Cannot apply a non-function"))
  ;;

  let eval_program_item (env : env) (item : structure_item)
    : (env * value option) eval_result
    =
    match item with
    | Expr e -> eval_expr env e >>= fun v -> return (env, Some v)
    | Value (NonRec, p, expr) ->
      eval_expr env expr
      >>= fun v ->
      (match p with
       | PVar name -> return (extend env name v, None)
       | PAny -> return (env, None))
    | Value (Rec, p, expr) ->
      (match p, expr with
       | PVar name, FunExpr (params, body) ->
         let rec_closure = VClosure (true, name, env, params, body) in
         let new_env = extend env name rec_closure in
         return (new_env, None)
       | PVar _, _ -> Error (TypeError "Recursive value definition must be a function")
       | PAny, _ -> eval_expr env expr >>= fun _ -> return (env, None))
  ;;

  let show_value = function
    | VInt i -> string_of_int i
    | VUnit -> "()"
    | VClosure _ -> "<fun>"
    | VBuiltin _ -> "<builtin>"
  ;;

  let show_error = function
    | UnboundVariable name -> "Error: Unbound variable " ^ name
    | TypeError msg -> "Error: Type error - " ^ msg
    | DivisionByZero -> "Error: Division by zero"
  ;;

  let initial_env : env =
    let open Base.Map in
    empty (module String)
    |> set ~key:"print_int"
      ~data:(VBuiltin
          (function
            | VInt i ->
              print_int i;
              return VUnit
            | _ -> Error (TypeError "print_int expects an integer")) )
    |> set ~key:"print_endl"
      ~data:(VBuiltin
          (function
            | VUnit ->
              print_newline ();
              return VUnit
            | _ -> Error (TypeError "print_endl expects a unit value ()")) )
    
  ;;
end
