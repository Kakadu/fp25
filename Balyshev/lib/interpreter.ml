open Base

module Error = struct
  type t =
    | Is_not_a_function of Parsetree.expression
    | Unbound_value of string
    | Type_mismatch of string
    | Division_by_zero
    | Not_implemented of string
    | LeftSideRec of string
    | RightSideRec of string
    | Match_failure

  open Format

  let show = function
    | Is_not_a_function expr ->
      sprintf
        "{ %s } is not a function, it can not be applied"
        (Parsetree.show_expression expr)
    | Unbound_value name -> sprintf "unbound value: { %s }" name
    | Type_mismatch msg -> sprintf "type mismatch: { %s }" msg
    | Division_by_zero -> sprintf "division by zero"
    | Not_implemented s -> sprintf "not implemented: %s" s
    | LeftSideRec patt_str -> sprintf "invalid left-hand side of let rec: { %s }" patt_str
    | RightSideRec name ->
      sprintf "invalid right-hand side of let rec in declaration of { %s }" name
    | Match_failure -> sprintf "match failure"
  ;;

  let pp ppf error = fprintf ppf "%s" (show error)
end

open Error

module Builtin (M : Monads.STATE_MONAD) = struct
  open M
  open Parsetree
  open Valuetree.Make (M) (Error)

  let fix =
    let efun x b = EFun (PVar x, b) in
    let x = EVar "x" in
    let y = EVar "y" in
    let u = efun "x" (EApp (EVar "f", efun "y" (EApp (EApp (x, x), y)))) in
    VFun (PVar "f", EApp (u, u), Base.Map.empty (module Base.String))
  ;;

  let print_value =
    let aux v =
      Format.printf "%a" pp_value v;
      return (VConstant CUnit)
    in
    vprimitive "print_value" aux
  ;;

  let printn_value =
    let aux v =
      Format.printf "%a\n" pp_value v;
      return (VConstant CUnit)
    in
    vprimitive "printn_value" aux
  ;;

  let env_with_primitives =
    Base.Map.empty (module Base.String)
    |> Map.set ~key:"fix" ~data:fix
    |> Map.set ~key:"print_value" ~data:print_value
    |> Map.set ~key:"printn_value" ~data:printn_value
  ;;
end

module Eval (M : Monads.STATE_MONAD) = struct
  module Builtin = Builtin (M)
  open Parsetree
  open Valuetree.Make (M) (Error)
  open M

  let from_env env key =
    match Map.find env key with
    | Some value -> return value
    | None -> fail (Unbound_value key)
  ;;

  let eval_int f a b = return (VConstant (CInt (f a b)))
  let eval_bool f a b = return (VConstant (CBool (f a b)))
  let update env name value = Map.update env name ~f:(fun _ -> value)

  let merge env1 env2 =
    Map.fold env2 ~f:(fun ~key ~data env -> update env key data) ~init:env1
  ;;

  let eval_binop op a b =
    match a, b with
    | VConstant a, VConstant b ->
      (match op, a, b with
       | Div, _, CInt 0 -> fail Division_by_zero
       | Add, CInt a, CInt b -> eval_int ( + ) a b
       | Sub, CInt a, CInt b -> eval_int ( - ) a b
       | Mul, CInt a, CInt b -> eval_int ( * ) a b
       | Div, CInt a, CInt b -> eval_int ( / ) a b
       | Eq, CInt a, CInt b -> eval_bool ( = ) a b
       | Ne, CInt a, CInt b -> eval_bool ( <> ) a b
       | Le, CInt a, CInt b -> eval_bool ( <= ) a b
       | Ge, CInt a, CInt b -> eval_bool ( >= ) a b
       | Lt, CInt a, CInt b -> eval_bool ( < ) a b
       | Gt, CInt a, CInt b -> eval_bool ( > ) a b
       | Eq, CBool a, CBool b -> eval_bool ( == ) a b
       | Ne, CBool a, CBool b -> eval_bool ( != ) a b
       | _ -> fail (Type_mismatch "eval_binop operands are not compatible"))
    | _ -> fail (Type_mismatch "eval_binop operands are not constants")
  ;;

  let rec bind_to_env env patt value =
    match patt, value with
    | PAny, _ -> return env
    | PVar x, value -> Map.set env ~key:x ~data:value |> return
    | PTuple (p1, p2, ps), VTuple (v1, v2, vs) when List.length ps = List.length vs ->
      bind_many_exn env (p1 :: p2 :: ps) (v1 :: v2 :: vs)
    | PConstruct (pname, Some parg), VConstruct (vname, Some varg)
      when String.equal pname vname -> bind_to_env env parg varg
    | PConstruct (pname, None), VConstruct (vname, None) when String.equal pname vname ->
      return env
    | PConstant CUnit, VConstant CUnit -> return env
    | PConstant (CInt x), VConstant (CInt y) when Int.equal x y -> return env
    | PConstant (CBool x), VConstant (CBool y) when Bool.equal x y -> return env
    | PConstant (CInt _), VConstant (CInt _)
    | PConstant (CBool _), VConstant (CBool _)
    | PConstruct _, VConstruct _ -> fail Match_failure
    | _ ->
      fail
        (Type_mismatch
           (Format.sprintf
              "{ value = %s } does not match { pattern = %s }"
              (show_value value)
              (show_pattern patt)))

  and bind_many_exn env patts values =
    let aux env patt value =
      let* env = env in
      bind_to_env env patt value
    in
    Base.List.fold2_exn ~init:(return env) ~f:aux patts values
  ;;

  let rec expression env = function
    | EVar name -> from_env env name
    | EConstant (_ as x) -> return (VConstant x)
    | EBinop (op, a, b) ->
      let* a = expression env a in
      let* b = expression env b in
      eval_binop op a b
    | ETuple (e1, e2, es) ->
      let* v1 = expression env e1 in
      let* v2 = expression env e2 in
      let* vs = expression_many env es in
      return (VTuple (v1, v2, vs))
    | EConstruct (name, None) -> return (VConstruct (name, None))
    | EConstruct (name, Some arg) ->
      let* arg = expression env arg in
      return (VConstruct (name, Some arg))
    | ELet (rec_flag, (vb1, vbs), body) ->
      let eval_vbs =
        match rec_flag with
        | NonRecursive -> bind_value_many
        | Recursive -> bind_rec_value_many
      in
      let* env = eval_vbs env (vb1, vbs) in
      expression env body
    | EApp (f, arg) ->
      let* varg = expression env arg in
      let* vfun = expression env f in
      (match vfun with
       | VFun (patt, body, closure) ->
         let env = merge env closure in
         let* env = bind_to_env env patt varg in
         let* rez = expression env body in
         return rez
       | VPrimitive (_name, f) -> f varg
       | _ -> fail (Is_not_a_function f))
    | EFun (patt, expr) -> return (VFun (patt, expr, env))
    | EIf (cond, expr_then, expr_else) ->
      expression env cond
      >>= (function
       | VConstant (CBool true) -> expression env expr_then
       | VConstant (CBool false) -> expression env expr_else
       | _ -> fail (Type_mismatch "boolean expr expected"))
    | EMatch (scrut, (case, cases)) ->
      let* scrut = expression env scrut in
      let f acc (patt, expr) =
        acc
        <|> let* env' = bind_to_env env patt scrut in
            expression env' expr
      in
      List.fold ~f ~init:(fail Match_failure) (case :: cases)

  and expression_many env exprs =
    let aux acc expr =
      let* values = acc in
      let* value = expression env expr in
      return (value :: values)
    in
    let* values = List.fold ~f:aux exprs ~init:(return []) in
    return (List.rev values)

  and bind_value_m env (patt, expr) =
    let* env = env in
    let* value = expression env expr in
    bind_to_env env patt value

  and bind_value_many env (vb1, vbs) =
    List.fold ~init:(return env) (vb1 :: vbs) ~f:bind_value_m

  and bind_rec_value_m env (patt, expr) =
    let* env = env in
    match patt, expr with
    | PVar fun_name, EFun (patt, expr) ->
      let value = VFun (patt, expr, env) in
      return (update env fun_name value)
    | PVar fun_name, _ -> fail (RightSideRec fun_name)
    | patt, _ -> fail (LeftSideRec (Parsetree.show_pattern patt))

  and bind_rec_value_many env (vb1, vbs) =
    List.fold ~init:(return env) (vb1 :: vbs) ~f:bind_rec_value_m
  ;;

  let stru_item env (patt, expr) =
    let* value = expression env expr in
    let* env = bind_to_env env patt value in
    return (env, (patt, value))
  ;;

  let stru_item_many env items =
    let aux acc item =
      let* env, items = acc in
      let* env, item = stru_item env item in
      return (env, item :: items)
    in
    let* env, items = List.fold items ~init:(return (env, [])) ~f:aux in
    return (env, List.rev items)
  ;;

  let structure env (vb1, vbs) =
    let* env, vb1 = stru_item env vb1 in
    let* _env, vbs = stru_item_many env vbs in
    return (vb1, List.rev vbs)
  ;;
end

module Make (M : Monads.STATE_MONAD) = struct
  module Builtin = Builtin (M)
  module Eval = Eval (M)

  let init_state = 0

  let eval_expression expr =
    match M.run (Eval.expression Builtin.env_with_primitives expr) init_state with
    | Ok (_state, value) -> Ok value
    | Error err -> Error err
  ;;

  let eval_structure vbs =
    match M.run (Eval.structure Builtin.env_with_primitives vbs) init_state with
    | Ok (_state, stru) -> Ok stru
    | Error err -> Error err
  ;;
end
