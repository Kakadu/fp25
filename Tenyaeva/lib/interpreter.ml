(** Copyright 2025, Tenyaeva Ekaterina *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Monad

type eval_error =
  | TypeError
  | DivisionByZero
  | MatchFailure
  | NoVariable of Ast.ident
  | OutOfSteps

let pp_eval_error ppf : eval_error -> unit =
  let open Format in
  function
  | TypeError -> fprintf ppf "Type error"
  | DivisionByZero -> fprintf ppf "Division by zero"
  | MatchFailure -> fprintf ppf "Matching failure"
  | NoVariable id -> fprintf ppf "Undefined variable '%s'" id
  | OutOfSteps -> fprintf ppf "Out of steps"
;;

type value =
  | ValInt of int
  | ValBool of bool
  | ValUnit
  | ValFun of Ast.rec_flag * Ast.pattern * Ast.expression * environment
  | ValFunction of Ast.case list * environment
  | ValOption of value option
  | ValBuiltin of Ast.ident

and environment = (Ast.ident, value, Base.String.comparator_witness) Base.Map.t

let rec pp_value ppf : value -> unit =
  let open Format in
  function
  | ValInt int -> fprintf ppf "%i" int
  | ValBool bool -> fprintf ppf "%b" bool
  | ValUnit -> fprintf ppf "()"
  | ValOption value ->
    (match value with
     | Some value -> fprintf ppf "Some %a" pp_value value
     | None -> fprintf ppf "None")
  | ValFun _ -> fprintf ppf "<fun>"
  | ValFunction _ -> fprintf ppf "<function>"
  | ValBuiltin _ -> fprintf ppf "<builtin>"
;;

module StepCounter = struct
  include StateR (struct
      type state = int
      type error = eval_error
    end)

  let tick =
    let* st = get in
    if st <= 0 then fail OutOfSteps else put (st - 1) >>| fun _ -> ()
  ;;
end

module Env = struct
  open Base
  open StepCounter

  let extend env key value = Map.update env key ~f:(fun _ -> value)

  let compose env1 env2 =
    Map.fold env2 ~f:(fun ~key ~data env_acc -> extend env_acc key data) ~init:env1
  ;;

  let find_exn env key =
    match Map.find env key with
    | Some value -> return value
    | None -> fail (NoVariable key)
  ;;

  let empty = Map.empty (module String)
  let env_with_print_funs = extend empty "print_int" (ValBuiltin "print_int")
end

module Eval = struct
  open StepCounter
  open Env

  let eval_un_op = function
    | Negative, ValInt val1 -> return (ValInt (-val1))
    | Positive, ValInt val1 -> return (ValInt val1)
    | Not, ValBool val1 -> return (ValBool (not val1))
    | _ -> fail TypeError
  ;;

  let eval_bin_op = function
    | Mult, ValInt val1, ValInt val2 -> return (ValInt (val1 * val2))
    | Div, ValInt val1, ValInt val2 when val2 <> 0 -> return (ValInt (val1 / val2))
    | Div, _, ValInt 0 -> fail DivisionByZero
    | Add, ValInt val1, ValInt val2 -> return (ValInt (val1 + val2))
    | Sub, ValInt val1, ValInt val2 -> return (ValInt (val1 - val2))
    | Gte, val1, val2 -> return (ValBool (val1 >= val2))
    | Lte, val1, val2 -> return (ValBool (val1 <= val2))
    | Neq, val1, val2 -> return (ValBool (val1 <> val2))
    | Eq, val1, val2 -> return (ValBool (val1 = val2))
    | Gt, val1, val2 -> return (ValBool (val1 > val2))
    | Lt, val1, val2 -> return (ValBool (val1 < val2))
    | _ -> fail TypeError
  ;;

  let rec match_pattern env = function
    | Pat_any, _ -> Some env
    | Pat_var id, value -> Some (extend env id value)
    | Pat_constant (Const_int pat), ValInt value when pat = value -> Some env
    | Pat_constant (Const_bool pat), ValBool value when pat = value -> Some env
    | Pat_constant Const_unit, _ -> Some env
    | Pat_constraint (_, pat), value -> match_pattern env (pat, value)
    | Pat_option None, ValOption None -> Some env
    | Pat_option (Some pat), ValOption (Some value) -> match_pattern env (pat, value)
    | _ -> None
  ;;

  let rec extend_names_from_pat (env : environment) = function
    | Pat_any, _ | Pat_constant Const_unit, ValUnit | Pat_option None, ValOption None ->
      return env
    | Pat_var id, value -> return (extend env id value)
    | Pat_constraint (_, pat), value | Pat_option (Some pat), ValOption (Some value) ->
      extend_names_from_pat env (pat, value)
    | _ -> fail TypeError
  ;;

  let rec eval_expression env ex =
    let* () = tick in
    match ex with
    | Expr_ident id -> find_exn env id
    | Expr_const const ->
      (match const with
       | Const_int int -> return (ValInt int)
       | Const_bool bool -> return (ValBool bool)
       | Const_unit -> return ValUnit)
    | Expr_let (NonRecursive, value_binding, value_binding_list, exp) ->
      let* env = eval_value_binding_list env (value_binding :: value_binding_list) in
      eval_expression env exp
    | Expr_let (Recursive, value_binding, value_binding_list, exp) ->
      let* env = eval_rec_value_binding_list env (value_binding :: value_binding_list) in
      eval_expression env exp
    | Expr_fun (pat, exp) -> return (ValFun (NonRecursive, pat, exp, env))
    | Expr_function (case, case_list) -> return (ValFunction (case :: case_list, env))
    | Expr_match (exp, case, case_list) ->
      let* match_value = eval_expression env exp in
      find_and_eval_case env match_value (case :: case_list)
    | Expr_binop (op, exp1, exp2) ->
      let* value1 = eval_expression env exp1 in
      let* value2 = eval_expression env exp2 in
      eval_bin_op (op, value1, value2)
    | Expr_unop (op, e) ->
      let* v = eval_expression env e in
      eval_un_op (op, v)
    | Expr_apply (exp1, exp2) ->
      let* fun_val = eval_expression env exp1 in
      let* arg_val = eval_expression env exp2 in
      (match fun_val with
       | ValFun (rec_flag, pat, exp, fun_env) ->
         let* new_env =
           match rec_flag, match_pattern fun_env (pat, arg_val) with
           | Recursive, Some extended_env -> return (compose env extended_env)
           | NonRecursive, Some extended_env -> return extended_env
           | _, None -> fail MatchFailure
         in
         eval_expression new_env exp
       | ValFunction (case_list, env) -> find_and_eval_case env arg_val case_list
       | ValBuiltin builtin ->
         (match builtin, arg_val with
          | "print_int", ValInt integer ->
            Format.printf "%d\n" integer;
            return ValUnit
          | _ -> fail TypeError)
       | _ -> fail TypeError)
    | Expr_option None -> return (ValOption None)
    | Expr_option (Some expr) ->
      let* value = eval_expression env expr in
      return (ValOption (Some value))
    | Expr_if (if_exp, then_exp, Some else_exp) ->
      let* value_if_exp = eval_expression env if_exp in
      (match value_if_exp with
       | ValBool true -> eval_expression env then_exp
       | ValBool false -> eval_expression env else_exp
       | _ -> fail TypeError)
    | Expr_if (fst_val, snd_val, None) ->
      let* value_fst_val = eval_expression env fst_val in
      (match value_fst_val with
       | ValBool true ->
         let* value_snd_val = eval_expression env snd_val in
         (match value_snd_val with
          | ValUnit as v -> return v
          | _ -> fail TypeError)
       | ValBool false -> return ValUnit
       | _ -> fail TypeError)
    | Expr_constraint (_, exp) -> eval_expression env exp

  and find_and_eval_case env value = function
    | [] -> fail MatchFailure
    | { case_pat; case_expr } :: tail ->
      let env_temp = match_pattern env (case_pat, value) in
      (match env_temp with
       | Some env -> eval_expression env case_expr
       | None -> find_and_eval_case env value tail)

  and eval_value_binding_list env value_binding_list =
    Base.List.fold_left
      ~f:(fun acc { vb_pat; vb_expr } ->
        let* env = acc in
        let* value = eval_expression env vb_expr in
        let* env = extend_names_from_pat env (vb_pat, value) in
        return env)
      ~init:(return env)
      value_binding_list

  and eval_rec_value_binding_list env value_binding_list =
    Base.List.fold_left
      ~f:(fun acc { vb_pat; vb_expr } ->
        let* env = acc in
        let* value = eval_expression env vb_expr in
        match vb_pat with
        | Pat_var name | Pat_constraint (_, Pat_var name) ->
          (match value with
           | ValFun (_, pat, expr, env) ->
             let value = ValFun (Recursive, pat, expr, env) in
             let env = extend env name value in
             return env
           | _ -> fail TypeError)
        | _ -> fail TypeError)
      ~init:(return env)
      value_binding_list
  ;;

  let eval_structure_item env out_list =
    let rec extract_names_from_pat (env : environment) acc = function
      | Pat_var id -> acc @ [ Some id, Base.Map.find_exn env id ]
      | Pat_constraint (_, pat) -> extract_names_from_pat env acc pat
      | Pat_option (Some pat) -> extract_names_from_pat env acc pat
      | _ -> acc
    in
    let get_names_from_let_binds env =
      Base.List.fold_left ~init:[] ~f:(fun acc { vb_pat; _ } ->
        extract_names_from_pat env acc vb_pat)
    in
    function
    | Str_eval exp ->
      let* val' = eval_expression env exp in
      let* () = tick in
      return (env, out_list @ [ None, val' ])
    | Str_value (NonRecursive, value_binding, value_binding_list) ->
      let value_binding_list = value_binding :: value_binding_list in
      let* env = eval_value_binding_list env value_binding_list in
      let eval_list = get_names_from_let_binds env value_binding_list in
      let* () = tick in
      return (env, out_list @ eval_list)
    | Str_value (Recursive, value_binding, value_binding_list) ->
      let value_binding_list = value_binding :: value_binding_list in
      let* env = eval_rec_value_binding_list env value_binding_list in
      let eval_list = get_names_from_let_binds env value_binding_list in
      let* () = tick in
      return (env, out_list @ eval_list)
  ;;

  let eval_structure env ast =
    let* env, out_list =
      Base.List.fold_left
        ~f:(fun acc item ->
          let* env, out_list = acc in
          let* env, out_list = eval_structure_item env out_list item in
          return (env, out_list))
        ~init:(return (env, []))
        ast
    in
    let remove_duplicates =
      let fun_equal el1 el2 =
        match el1, el2 with
        | (Some id1, _), (Some id2, _) -> String.equal id1 id2
        | _ -> false
      in
      function
      | x :: xs when not (Base.List.mem xs x ~equal:fun_equal) -> x :: xs
      | _ :: xs -> xs
      | [] -> []
    in
    return (env, remove_duplicates out_list)
  ;;
end

let run_interpreter structure n =
  match StepCounter.run (Eval.eval_structure Env.env_with_print_funs structure) n with
  | _state, Ok (_env, value) -> Ok value
  | _state, Error err -> Error err
;;
