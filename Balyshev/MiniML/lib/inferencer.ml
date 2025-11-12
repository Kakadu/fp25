open Parsetree
open Typedtree

type error =
  | Not_implemented of string
  | Occurs_check of Ident.t * ty
  | Unification_failed of ty * ty
  | Type_mismatch
  | Unbound_value of string

let show_error = function
  | Not_implemented f -> Format.sprintf "not implemented: %s" f
  | Occurs_check (ident, ty) -> Format.sprintf "%s occurs in %s" ident.name (show_ty ty)
  | Unification_failed (a, b) ->
    Format.sprintf "unification of %s and %s failed" (show_ty a) (show_ty b)
  | Type_mismatch -> "type mismatch"
  | Unbound_value name -> Format.sprintf "unbound value: %s" name
;;

let pp_error ppf err = Format.fprintf ppf "%s" (show_error err)

type subsitution = ty Ident.Ident_map.t

module State (M : Monads.STATE_MONAD) = struct
  type t =
    { cnt : int
    ; sub : subsitution
    }

  let init_state = { cnt = 0; sub = Ident.Ident_map.empty }
end

module Substitution (M : Monads.STATE_MONAD) = struct
  open State (M)
  open Ident
  open Base
  open M

  type t = subsitution

  let occurs_in ident = function
    | Tty_var _ -> false
    | ty ->
      let rec helper = function
        | Tty_constr (params, _) -> List.exists ~f:helper params
        | Tty_var ident2 -> Ident.equal ident ident2
        | Tty_prod (ty1, ty2, tys) -> List.exists ~f:helper (ty1 :: ty2 :: tys)
        | Tty_arrow (ty1, ty2) -> helper ty1 || helper ty2
      in
      helper ty
  ;;

  let rec apply = function
    | Tty_constr (params, name) ->
      return (fun params -> Tty_constr (params, name)) <*> apply_fold params
    | Tty_var ident ->
      let* state = get in
      (match Ident_map.find_by_id_opt ident.id state.sub with
       | Some ty -> apply ty
       | None -> return (Tty_var ident))
    | Tty_prod (ty1, ty2, tys) ->
      return (fun ty1 ty2 tys -> Tty_prod (ty1, ty2, tys))
      <*> apply ty1
      <*> apply ty2
      <*> apply_fold tys
    | Tty_arrow (ty1, ty2) ->
      return (fun ty1 ty2 -> Tty_arrow (ty1, ty2)) <*> apply ty1 <*> apply ty2

  and apply_fold tys =
    List.fold tys ~init:(return []) ~f:(fun acc ty ->
      let* tys = acc in
      let* ty' = apply ty in
      return (List.rev (ty' :: tys)))
  ;;

  let rec unify ty1 ty2 =
    let* ty1' = apply ty1 in
    let* ty2' = apply ty2 in
    match ty1', ty2' with
    | Tty_constr (params1, ident1), Tty_constr (params2, ident2)
      when Ident.equal ident1 ident2 -> unify_lists params1 params2 (ty1, ty2)
    | Tty_var ident1, Tty_var ident2 when Ident.equal ident1 ident2 -> return ()
    | Tty_var ident1, ty when occurs_in ident1 ty -> fail (Occurs_check (ident1, ty))
    | ty, Tty_var name when occurs_in name ty -> fail (Occurs_check (name, ty))
    | Tty_var ident, ty | ty, Tty_var ident ->
      let* state = get in
      put { state with sub = Ident_map.add ident ty state.sub }
    | Tty_prod (x1, x2, xs), Tty_prod (y1, y2, ys) ->
      unify_lists (x1 :: x2 :: xs) (y1 :: y2 :: ys) (ty1, ty2)
    | Tty_arrow (a1, b1), Tty_arrow (a2, b2) ->
      let* () = unify a1 a2 in
      let* () = unify b1 b2 in
      return ()
    | _ -> fail (Unification_failed (ty1, ty2))

  and unify_lists tys1 tys2 (ty1, ty2) =
    match tys1, tys2 with
    | [], [] -> return ()
    | ty1 :: rest1, ty2 :: rest2 ->
      let* ty1' = apply ty1 in
      let* ty2' = apply ty2 in
      let* () = unify ty1' ty2' in
      unify_lists rest1 rest2 (ty1, ty2)
    | _ -> fail (Unification_failed (ty1, ty2))
  ;;

  (* TODO : test it *)
  let extend ident ty =
    let* state = get in
    put { state with sub = Ident.Ident_map.add ident ty state.sub }
  ;;
end

module Scheme (M : Monads.STATE_MONAD) = struct
  module Substitution = Substitution (M)
  include Typedtree.Scheme
end

module TypeEnv (M : Monads.STATE_MONAD) = struct
  include Typedtree.TypeEnv
  module Substitution = Substitution (M)

  type 's env = ('s, TypeEnv.t, error) M.t

  open M

  let add_value_m name scheme (env : _ env) =
    let* env = env in
    let* id = fresh_int in
    let ident = Ident.ident id name in
    return (add_value ~ident scheme env)
  ;;

  let add_type_m name tty_params tty_kind (env : _ env) =
    let* env = env in
    let* id = fresh_int in
    let tty_ident = Ident.ident id name in
    return (add_type { tty_ident; tty_params; tty_kind } env)
  ;;

  let get_arity = function
    | None -> 0
    | Some (Tty_prod (_, _, tys)) -> List.length tys + 2
    | Some _ -> 1
  ;;

  let add_constructor_m name constr_arg_ty ~type_ident:constr_type_ident (env : _ env) =
    let* env = env in
    let* id = fresh_int in
    let constr_ident = Ident.ident id name in
    let constr_arity = get_arity constr_arg_ty in
    let env =
      add_constructor { constr_arg_ty; constr_type_ident; constr_ident; constr_arity } env
    in
    return env
  ;;

  let base_types_env : _ env =
    return TypeEnv.empty
    |> add_type_m "unit" VarSet.empty (Tty_abstract None)
    |> add_type_m "bool" VarSet.empty (Tty_abstract None)
    |> add_type_m "int" VarSet.empty (Tty_abstract None)
  ;;

  (* TODO : REWRITE THIS *)

  let unit_ident = Ident.ident 1000 "unit"
  let int_ident = Ident.ident 1001 "int"
  let bool_ident = Ident.ident 1002 "bool"
  let ty_unit = Tty_constr ([], unit_ident)
  let ty_int = Tty_constr ([], int_ident)
  let ty_bool = Tty_constr ([], bool_ident)

  let base_types_env =
    TypeEnv.empty
    |> add_type
         { tty_kind = Tty_abstract None
         ; tty_ident = unit_ident
         ; tty_params = VarSet.empty
         }
    |> add_type
         { tty_kind = Tty_abstract None
         ; tty_ident = int_ident
         ; tty_params = VarSet.empty
         }
    |> add_type
         { tty_kind = Tty_abstract None
         ; tty_ident = bool_ident
         ; tty_params = VarSet.empty
         }
  ;;

  let apply _ = failwith "not implemented"
end

module Infer (M : Monads.STATE_MONAD) = struct
  module TypeEnv = TypeEnv (M)
  module Substitution = Substitution (M)
  module State = State (M)
  open State
  open M

  let fresh_tvar_gen =
    let* state = get in
    let ident = Ident.ident state.cnt ("'_" ^ string_of_int state.cnt) in
    let* () = put { state with cnt = state.cnt + 1 } in
    return (Tty_var ident)
  ;;

  let fresh_tvar_name name =
    let* state = get in
    let ident = Ident.ident state.cnt name in
    let* () = put { state with cnt = state.cnt + 1 } in
    return (Tty_var ident)
  ;;

  let rec pattern env = function
    | PAny ->
      let* ty = fresh_tvar_gen in
      return (env, ty)
    | PVar name ->
      let* state = get in
      let ident = Ident.ident state.cnt name in
      let* () = put { state with cnt = state.cnt + 1 } in
      let ty = Tty_var ident in
      let scheme = Typedtree.Scheme.Scheme (VarSet.empty, ty) in
      let env' = TypeEnv.add_value ~ident scheme env in
      return (env', ty)
    | PTuple (p1, p2, ps) ->
      let helper acc patt =
        let* env, tys = acc in
        let* env, ty = pattern env patt in
        return (env, ty :: tys)
      in
      let* env, ty1 = pattern env p1 in
      let* env, ty2 = pattern env p2 in
      let* env, tys = Base.List.fold ps ~init:(return (env, [])) ~f:helper in
      return (env, Tty_prod (ty1, ty2, tys))
    | _ -> fail (Not_implemented "pattern")
  ;;

  open TypeEnv

  let type_of_binop = function
    | Add | Sub | Mul | Div -> return (ty_int, ty_int, ty_int)
    | Eq | Ne | Le | Ge | Lt | Gt -> return (ty_bool, ty_int, ty_int)
    | _ -> fail (Not_implemented "type of binop")
  ;;

  let rec expression env = function
    | EConstant CUnit -> return ty_unit
    | EConstant (CBool _) -> return ty_bool
    | EConstant (CInt _) -> return ty_int
    | EVar name ->
      (* TODO : REWRITE THIS  *)
      (match Ident.Ident_map.find_by_name_opt name env.env_values with
       | Some (Typedtree.Scheme.Scheme (_, ty)) -> return ty
       | None ->
         let* state = get in
         let id = state.cnt in
         let* () = put { state with cnt = state.cnt + 1 } in
         let ident = Ident.ident id name in
         let ty = Tty_var ident in
         let* () = Substitution.extend ident ty in
         return ty)
    (* ALL OF THIS *)
    | EBinop (op, e1, e2) ->
      let* ty1 = expression env e1 in
      (* TODO : decide if it matters *)
      (* let* env = TypeEnv.apply env in *)
      let* ty2 = expression env e2 in
      let* ty, left_ty, right_ty = type_of_binop op in
      let* ty1' = Substitution.apply ty1 in
      let* ty2' = Substitution.apply ty2 in
      let* () = Substitution.unify ty1' left_ty in
      let* () = Substitution.unify ty2' right_ty in
      let* ty = Substitution.apply ty in
      return ty
    | ETuple (e1, e2, es) ->
      let* ty1 = expression env e1 in
      let* ty2 = expression env e2 in
      let* tys =
        Base.List.fold es ~init:(return []) ~f:(fun acc e ->
          let* tys = acc in
          let* ty = expression env e in
          return (ty :: tys))
      in
      let* ty1' = Substitution.apply ty1 in
      let* ty2' = Substitution.apply ty2 in
      let* tys' = Substitution.apply_fold (List.rev tys) in
      let ty = Tty_prod (ty1', ty2', tys') in
      return ty
    | EApp (e1, e2) ->
      let* ty1 = expression env e1 in
      let* ty2 = expression env e2 in
      let* ty1' = Substitution.apply ty1 in
      let* ty2' = Substitution.apply ty2 in
      let* tv = fresh_tvar_gen in
      let* () = Substitution.unify ty1' (Tty_arrow (ty2', tv)) in
      let* ty = Substitution.apply tv in
      return ty
    | EFun (p, e) ->
      let* env, ty1 = pattern env p in
      let* ty2 = expression env e in
      let ty12 = Tty_arrow (ty1, ty2) in
      let* ty = Substitution.apply ty12 in
      return ty
    | EIf (cond, expr_then, expr_else) ->
      let* ty1 = expression env cond in
      let* () = Substitution.unify ty1 ty_bool in
      let* ty2 = expression env expr_then in
      let* ty3 = expression env expr_else in
      let* () = Substitution.unify ty2 ty3 in
      let* ty = Substitution.apply ty2 in
      return ty
    | ELet (NonRecursive, (vb, vbs), body) ->
      let helper env (patt, expr) =
        let* env = env in
        let* env, ty1 = pattern env patt in
        let* ty2 = expression env expr in
        let* () = Substitution.unify ty1 ty2 in
        TypeEnv.apply env
      in
      let* env = Base.List.fold (vb :: vbs) ~f:helper ~init:(return env) in
      expression env body
    | EMatch (subject, ((p1, e1), cases)) ->
      let helper acc (patt, expr) =
        let* ty_patt_acc, ty_expr_acc = acc in
        let* env, ty_patt = pattern env patt in
        let* ty_expr = expression env expr in
        let* () = Substitution.unify ty_expr_acc ty_expr in
        let* () = Substitution.unify ty_patt_acc ty_patt in
        let* ty_patt = Substitution.apply ty_patt in
        let* ty_expr = Substitution.apply ty_expr in
        return (ty_patt, ty_expr)
      in
      let* ty_expr = expression env subject in
      let* env1, ty_p1 = pattern env p1 in
      let* ty_e1 = expression env1 e1 in
      let* ty_patt_cases, ty_expr_cases =
        Base.List.fold ~f:helper ~init:(return (ty_p1, ty_e1)) cases
      in
      let* () = Substitution.unify ty_expr ty_patt_cases in
      let* ty = Substitution.apply ty_expr_cases in
      return ty
    | _ -> fail (Not_implemented "expression")
  ;;

  let expression ast =
    match M.run (expression TypeEnv.empty ast) State.init_state with
    | Ok (_state, ty) -> Ok ty
    | Error e -> Error e
  ;;
end
