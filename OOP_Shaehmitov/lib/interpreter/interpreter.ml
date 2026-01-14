[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast
open Base

module type Eval = sig
  type value
  type error
  type env
  type 'a eval_result = ('a, error) Result.t

  val show_error : error -> string
  val run_program : int -> program -> (env, error) Result.t
end

module Interpreter : Eval = struct
  type error =
    | UnboundVariable of name
    | UnboundClass of name
    | UnboundMethod of name
    | TypeError of string
    | DivisionByZero
    | OutOfMaxSteps

  type 'a eval_result = ('a, error) Result.t
  type is_rec = bool

  module Env = Stdlib.Map.Make (String)

  type value =
    | VInt of int
    | VUnit
    | VClosure of is_rec * name * env * pattern list * expr
    | VBuiltin of (value -> value eval_result)
    | VBool of bool
    | VTuple of value list
    | VClass of class_def
    | Vobject of object_val

  and object_val =
    { cls_def : class_def
    ; fields : value Env.t
    }

  and env = value Env.t

  let ( let* ) res f =
    match res with
    | Ok v -> f v
    | Error e -> Error e
  ;;

  let return v = Ok v

  let lookup_env (env : env) (x : name) : value eval_result =
    match Env.find_opt x env with
    | Some v -> return v
    | None -> Error (UnboundVariable x)
  ;;

  let extend env key value = Env.add key value env

  let lookup_class (env : env) (name : name) : class_def eval_result =
    match Env.find_opt name env with
    | Some (VClass c) -> return c
    | Some _ -> Error (TypeError ("Expected a class for " ^ name))
    | None -> Error (UnboundClass name)
  ;;

  let rec bind_pattern (p : pattern) (v : value) (env : env) : env eval_result =
    match p, v with
    | PAny, _ -> return env
    | PVar name, _ -> return (extend env name v)
    | PUnit, VUnit -> return env
    | PTuple ps, VTuple vs ->
      if Stdlib.List.length ps <> Stdlib.List.length vs
      then Error (TypeError "Tuple length mismatch in binding")
      else
        Stdlib.List.fold_left2
          (fun acc_res p_elem v_elem ->
            let* acc_env = acc_res in
            bind_pattern p_elem v_elem acc_env)
          (return env)
          ps
          vs
    | _, _ -> Error (TypeError "Pattern binding failed")
  ;;

  let rec collect_fields
    (eval : env -> expr -> int -> (value * int) eval_result)
    (global_env : env)
    (cls : class_def)
    (param_env : env)
    (steps : int)
    : (env * int) eval_result
    =
    let* parent_fields_env, steps =
      match cls.parent_class with
      | None -> return (Env.empty, steps)
      | Some (parent_name, args) ->
        let* parent_cls = lookup_class global_env parent_name in
        let* arg_vals, steps =
          Stdlib.List.fold_left
            (fun acc arg ->
              let* vals, s = acc in
              let* v, s' = eval param_env arg s in
              return (vals @ [ v ], s'))
            (return ([], steps))
            args
        in
        if Stdlib.List.length parent_cls.class_params <> Stdlib.List.length arg_vals
        then Error (TypeError "Wrong number of args for parent")
        else
          let* parent_param_env =
            Stdlib.List.fold_left2
              (fun acc p v ->
                let* e = acc in
                bind_pattern p v e)
              (return global_env)
              parent_cls.class_params
              arg_vals
          in
          collect_fields eval global_env parent_cls parent_param_env steps
    in
    Stdlib.List.fold_left
      (fun acc (name, expr) ->
        let* env_acc, s = acc in
        let* v, s' = eval param_env expr s in
        let unique_name = cls.class_name ^ "#" ^ name in
        return (Env.add unique_name v env_acc, s'))
      (return (parent_fields_env, steps))
      cls.fields
  ;;

  let rec lookup_method (env : env) (cls : class_def) (method_name : name)
    : (method_def * name) eval_result
    =
    match
      Stdlib.List.find_opt (fun m -> String.equal m.method_name method_name) cls.methods
    with
    | Some m -> return (m, cls.class_name)
    | None ->
      (match cls.parent_class with
       | Some (parent_name, _) ->
         let* parent_cls = lookup_class env parent_name in
         lookup_method env parent_cls method_name
       | None -> Error (UnboundMethod method_name))
  ;;

  let get_ancestry_chain (env : env) (start_cls_name : name) : class_def list eval_result =
    let rec loop curr_name acc =
      let* cls = lookup_class env curr_name in
      match cls.parent_class with
      | None -> return (cls :: acc)
      | Some (p_name, _) -> loop p_name (cls :: acc)
    in
    loop start_cls_name []
  ;;

  let rec eval_expr (env : env) (e : expr) (steps : int) : (value * int) eval_result =
    if steps <= 0
    then Error OutOfMaxSteps
    else (
      let steps = steps - 1 in
      match e with
      | Const (CInt i) -> return (VInt i, steps)
      | Const (CBool b) -> return (VBool b, steps)
      | Const CUnit -> return (VUnit, steps)
      | Var name ->
        let* v = lookup_env env name in
        return (v, steps)
      | UnOp (Neg, e) ->
        let* v, steps = eval_expr env e steps in
        (match v with
         | VInt i -> return (VInt (-i), steps)
         | _ -> Error (TypeError "neg"))
      | UnOp (Not, e) ->
        let* v, steps = eval_expr env e steps in
        (match v with
         | VBool b -> return (VBool (not b), steps)
         | _ -> Error (TypeError "not"))
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
            | Eq -> return (VBool (i1 = i2), steps)
            | Neq -> return (VBool (i1 <> i2), steps)
            | Lt -> return (VBool (i1 < i2), steps)
            | Le -> return (VBool (i1 <= i2), steps)
            | Gt -> return (VBool (i1 > i2), steps)
            | Ge -> return (VBool (i1 >= i2), steps)
            | _ -> Error (TypeError "int op"))
         | VBool b1, VBool b2 ->
           (match op with
            | And -> return (VBool (b1 && b2), steps)
            | Or -> return (VBool (b1 || b2), steps)
            | _ -> Error (TypeError "bool op"))
         | _ -> Error (TypeError "bin op mismatch"))
      | If (cond, t, f) ->
        let* v, steps = eval_expr env cond steps in
        (match v with
         | VBool true -> eval_expr env t steps
         | VBool false -> eval_expr env f steps
         | _ -> Error (TypeError "if"))
      | FunExpr (params, body) -> return (VClosure (false, "", env, params, body), steps)
      | Let (NonRec, p, e1, e2) ->
        let* v1, steps = eval_expr env e1 steps in
        let* new_env = bind_pattern p v1 env in
        eval_expr new_env e2 steps
      | Let (Rec, p, e1, e2) ->
        (match p, e1 with
         | PVar name, FunExpr (params, body) ->
           let rec_closure = VClosure (true, name, env, params, body) in
           let new_env = extend env name rec_closure in
           eval_expr new_env e2 steps
         | _ -> Error (TypeError "rec binding"))
      | App (f, arg) ->
        let* func, steps = eval_expr env f steps in
        let* arg_val, steps = eval_expr env arg steps in
        (match func with
         | VClosure (rec_flag, name, closure_env, params, body) ->
           let closure_env =
             if rec_flag then extend closure_env name func else closure_env
           in
           (match params with
            | [] -> Error (TypeError "app empty params")
            | p :: rest ->
              let* new_env = bind_pattern p arg_val closure_env in
              if List.is_empty rest
              then eval_expr new_env body steps
              else return (VClosure (false, name, new_env, rest, body), steps))
         | VBuiltin builtin_fn ->
           let* res = builtin_fn arg_val in
           return (res, steps)
         | _ -> Error (TypeError "app non-func"))
      | Tuple es ->
        let* vals, steps =
          Stdlib.List.fold_left
            (fun acc e ->
              let* vs, s = acc in
              let* v, s' = eval_expr env e s in
              return (vs @ [ v ], s'))
            (return ([], steps))
            es
        in
        return (VTuple vals, steps)
      | New (class_name, args) ->
        let* cls = lookup_class env class_name in
        let* arg_vals, steps =
          Stdlib.List.fold_left
            (fun acc arg ->
              let* vals, s = acc in
              let* v, s' = eval_expr env arg s in
              return (vals @ [ v ], s'))
            (return ([], steps))
            args
        in
        if Stdlib.List.length cls.class_params <> Stdlib.List.length arg_vals
        then Error (TypeError "new args mismatch")
        else
          let* class_env =
            Stdlib.List.fold_left2
              (fun acc_env param arg ->
                let* e = acc_env in
                bind_pattern param arg e)
              (return env)
              cls.class_params
              arg_vals
          in
          let* fields_env, steps = collect_fields eval_expr env cls class_env steps in
          return (Vobject { cls_def = cls; fields = fields_env }, steps)
      | MethodCall (obj_expr, method_name, args) ->
        let* obj_val, steps = eval_expr env obj_expr steps in
        (match obj_val with
         | Vobject { cls_def = obj_cls_def; fields } ->
           let* method_def, defining_cls_name =
             lookup_method env obj_cls_def method_name
           in
           let* arg_vals, steps =
             Stdlib.List.fold_left
               (fun acc arg ->
                 let* vals, s = acc in
                 let* v, s' = eval_expr env arg s in
                 return (vals @ [ v ], s'))
               (return ([], steps))
               args
           in
           if Stdlib.List.length method_def.method_params <> Stdlib.List.length arg_vals
           then Error (TypeError "method args mismatch")
           else (
             let method_env =
               match obj_cls_def.self_name with
               | Some self -> extend env self obj_val
               | None -> env
             in
             let* chain = get_ancestry_chain env defining_cls_name in
             let method_env_with_fields =
               Stdlib.List.fold_left
                 (fun curr_env ancestral_cls ->
                   let prefix = ancestral_cls.class_name ^ "#" in
                   Env.fold
                     (fun key value acc ->
                       if Stdlib.String.starts_with ~prefix key
                       then (
                         let short_name =
                           Stdlib.String.sub
                             key
                             (String.length prefix)
                             (String.length key - String.length prefix)
                         in
                         Env.add short_name value acc)
                       else acc)
                     fields
                     curr_env)
                 method_env
                 chain
             in
             let* final_method_env =
               Stdlib.List.fold_left2
                 (fun acc_env p v ->
                   let* e = acc_env in
                   bind_pattern p v e)
                 (return method_env_with_fields)
                 method_def.method_params
                 arg_vals
             in
             eval_expr final_method_env method_def.method_body steps)
         | _ -> Error (TypeError "method call on non-object")))
  ;;

  let eval_program_item (env : env) (item : structure_item) (steps : int)
    : (env * int) eval_result
    =
    match item with
    | Value (NonRec, p, expr) ->
      let* v, steps = eval_expr env expr steps in
      let* new_env = bind_pattern p v env in
      return (new_env, steps)
    | Value (Rec, p, expr) ->
      (match p, expr with
       | PVar name, FunExpr (params, body) ->
         let rec_closure = VClosure (true, name, env, params, body) in
         let new_env = extend env name rec_closure in
         return (new_env, steps)
       | _ -> Error (TypeError "rec def"))
    | ClassDef cls_def ->
      let new_env = extend env cls_def.class_name (VClass cls_def) in
      return (new_env, steps)
  ;;

  let show_error = function
    | UnboundVariable name -> "Error: Unbound variable " ^ name
    | TypeError msg -> "Error: Type error - " ^ msg
    | DivisionByZero -> "Error: Division by zero"
    | OutOfMaxSteps -> "Error: Out of maximum allowed evaluation steps"
    | UnboundClass name -> "Error: Unbound class " ^ name
    | UnboundMethod name -> "Error: Unbound method " ^ name
  ;;

  let initial_env : env =
    Env.empty
    |> Env.add
         "print_int"
         (VBuiltin
            (function
              | VInt i ->
                Stdlib.print_int i;
                return VUnit
              | _ -> Error (TypeError "p_int")))
    |> Env.add
         "print_endl"
         (VBuiltin
            (function
              | VUnit ->
                Stdlib.print_newline ();
                return VUnit
              | _ -> Error (TypeError "p_unit")))
    |> Env.add
         "print_bool"
         (VBuiltin
            (function
              | VBool b ->
                Stdlib.print_string (if b then "true" else "false");
                return VUnit
              | _ -> Error (TypeError "p_bool")))
  ;;

  let run_program (max_steps : int) (prog : program) =
    let* final_env, _ =
      Stdlib.List.fold_left
        (fun acc item ->
          let* env, steps = acc in
          let* new_env, steps' = eval_program_item env item steps in
          return (new_env, steps'))
        (return (initial_env, max_steps))
        prog
    in
    return final_env
  ;;
end
