open Parsetree
open Typedtree

type error =
  | Not_implemented of string
  | Occurs_check of ty * ty
  | Unification_failed of ty * ty
  | Type_mismatch
  | Unbound_value of string
  | Type_was_not_declared of string
  | Type_arity_mismatch of string (* TODO : change message *)
  | Unbound_type_variable of string
  | Type_param_duplicates of string
  | Unbound_constructor of string
  | Type_env_invariant_violation of string
  | Constructor_arity_mismatch of string

let show_error = function
  | Not_implemented f -> Format.sprintf "not implemented: %s" f
  | Occurs_check (ty1, ty2) ->
    Format.sprintf "%s occurs in %s" (show_ty ty1) (show_ty ty2)
  | Unification_failed (a, b) ->
    Format.sprintf "unification of %s and %s failed" (show_ty a) (show_ty b)
  | Type_mismatch -> "type mismatch"
  | Unbound_value name -> Format.sprintf "unbound value: %s" name
  | Type_was_not_declared name -> Format.sprintf "type %s was not declared" name
  | Type_arity_mismatch name -> Format.sprintf "type arity mistmatch: %s" name
  | Unbound_type_variable name -> Format.sprintf "unbound type variable: %s" name
  | Type_param_duplicates name ->
    Format.sprintf "type param duplicates in declaration of type %s" name
  | Unbound_constructor name -> Format.sprintf "unbound constructor: %s" name
  | Type_env_invariant_violation msg -> msg
  | Constructor_arity_mismatch name ->
    Format.sprintf "constructor arity mismatch: %s" name
;;

let log msg = Format.printf "<< %s >>@ " msg
let pp_error ppf err = Format.fprintf ppf "%s" (show_error err)

type subsitution = ty Ident.Ident_map.t

module State (M : Monads.STATE_MONAD) = struct
  type t =
    { cnt : int
    ; sub : subsitution
    }

  let init_state = { cnt = 0; sub = Ident.Ident_map.empty }

  open M

  let gen_fresh_int =
    let* state = get in
    let rez = state.cnt in
    let* () = put { state with cnt = state.cnt + 1 } in
    return rez
  ;;
end

module VarSet = struct
  include Typedtree.VarSet

  let free_vars ty =
    let rec helper var_set = function
      | Tty_var ident -> VarSet.add ident var_set
      | Tty_arrow (ty1, ty2) -> helper (helper var_set ty1) ty2
      | Tty_prod (ty1, ty2, tys) ->
        Base.List.fold ~f:helper ~init:var_set (ty1 :: ty2 :: tys)
      | Tty_constr (tys, _ident) -> Base.List.fold ~f:helper ~init:var_set tys
    in
    match ty with
    | Tty_var _ -> VarSet.empty
    | ty -> helper VarSet.empty ty
  ;;
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
       | Some ty -> return ty
       | None -> return (Tty_var ident))
    | Tty_prod (ty1, ty2, tys) ->
      return (fun ty1 ty2 tys -> Tty_prod (ty1, ty2, tys))
      <*> apply ty1
      <*> apply ty2
      <*> apply_fold tys
    | Tty_arrow (ty1, ty2) ->
      return (fun ty1 ty2 -> Tty_arrow (ty1, ty2)) <*> apply ty1 <*> apply ty2

  and apply_fold tys =
    let* tys =
      List.fold tys ~init:(return []) ~f:(fun acc ty ->
        let* tys = acc in
        let* ty' = apply ty in
        return (ty' :: tys))
    in
    return (List.rev tys)
  ;;

  let rec unify ty1 ty2 =
    let* ty1' = apply ty1 in
    let* ty2' = apply ty2 in
    match ty1', ty2' with
    | Tty_constr (params1, ident1), Tty_constr (params2, ident2)
      when Ident.equal ident1 ident2 && List.length params1 = List.length params2 ->
      unify_lists params1 params2
    | Tty_var ident1, Tty_var ident2 when Ident.equal ident1 ident2 -> return ()
    | Tty_var ident, ty when occurs_in ident ty -> fail (Occurs_check (ty1', ty))
    | ty, Tty_var ident when occurs_in ident ty -> fail (Occurs_check (ty2', ty))
    | Tty_var ident, ty | ty, Tty_var ident ->
      let* ty' = apply ty in
      let* state = get in
      put { state with sub = Ident_map.add ident ty' state.sub }
    | Tty_prod (x1, x2, xs), Tty_prod (y1, y2, ys) when List.length xs = List.length ys ->
      unify_lists (x1 :: x2 :: xs) (y1 :: y2 :: ys)
    | Tty_arrow (a1, b1), Tty_arrow (a2, b2) ->
      let* () = unify a1 a2 in
      let* () = unify b1 b2 in
      return ()
    | _ -> fail (Unification_failed (ty1', ty2'))

  and unify_lists tys1 tys2 =
    List.fold2_exn
      ~f:(fun acc ty1 ty2 ->
        let* () = acc in
        unify ty1 ty2)
      ~init:(return ())
      tys1
      tys2
  ;;

  (* TODO : test it *)
  let extend ident ty =
    let* state = get in
    put { state with sub = Ident.Ident_map.add ident ty state.sub }
  ;;

  let remove ident =
    let* state = get in
    put { state with sub = Ident.Ident_map.remove ident state.sub }
  ;;
end

module Scheme (M : Monads.STATE_MONAD) = struct
  module Substitution = Substitution (M)
  open M
  open State (M)
  include Typedtree.Scheme

  (* TODO : REWRITE THIS IMPERATIVE PEACE OF **** *)
  let apply (Scheme (var_set, ty)) =
    let* state = get in
    let sub = state.sub in
    let cnt = state.cnt in
    let helper ident acc =
      let* () = acc in
      Substitution.remove ident
    in
    let* () = VarSet.fold helper var_set (return ()) in
    let* ty' = Substitution.apply ty in
    let* () = put { cnt; sub } in
    return (Scheme (var_set, ty'))
  ;;

  let free_vars (Scheme.Scheme (var_set, ty)) = VarSet.diff var_set (VarSet.free_vars ty)
end

module TypeEnv (M : Monads.STATE_MONAD) = struct
  include Typedtree.TypeEnv
  module Substitution = Substitution (M)
  module Scheme = Scheme (M)
  module State = State (M)

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

  (* TODO : REWRITE THIS *)

  let unit_ident = Ident.ident 1000 "unit"
  let int_ident = Ident.ident 1001 "int"
  let bool_ident = Ident.ident 1002 "bool"
  let ty_unit = Tty_constr ([], unit_ident)
  let ty_int = Tty_constr ([], int_ident)
  let ty_bool = Tty_constr ([], bool_ident)

  let base_types_env =
    TypeEnv.empty
    |> add_type { tty_kind = Tty_abstract None; tty_ident = unit_ident; tty_params = [] }
    |> add_type { tty_kind = Tty_abstract None; tty_ident = int_ident; tty_params = [] }
    |> add_type { tty_kind = Tty_abstract None; tty_ident = bool_ident; tty_params = [] }
  ;;

  module Ident_map_m = Ident.Ident_map_m (M)

  (* TODO : apply sub to types and constructors *)
  let apply env =
    let* env_values = Ident_map_m.fmapm ~f:Scheme.apply env.env_values in
    return { env with env_values }
  ;;

  let free_vars env =
    let helper _id scheme var_set = VarSet.union var_set (Scheme.free_vars scheme) in
    Ident.Ident_map.fold_by_ids ~f:helper env.env_values ~init:VarSet.empty
  ;;

  let find_constructor name env =
    match Ident.Ident_map.find_by_name_opt name env.env_constructors with
    | None -> fail (Unbound_constructor name)
    | Some constr_entry ->
      let ty_ident = constr_entry.constr_type_ident in
      (match Ident.Ident_map.find_by_id_opt ty_ident.id env.env_types with
       | None ->
         fail
           (Type_env_invariant_violation
              "constructor references a non-existent type ident")
       | Some type_entry -> return (constr_entry, type_entry))
  ;;

  let rec extend env pattern (scheme : Scheme.t) =
    match pattern, scheme with
    | PVar name, scheme ->
      let* fresh = State.gen_fresh_int in
      let ident = Ident.ident fresh name in
      let env = add_value ~ident scheme env in
      return env
    | PTuple (p1, p2, ps), Scheme (var_set, Tty_prod (ty1, ty2, tys))
      when List.length ps = List.length tys ->
      let helper acc p ty =
        let* env = acc in
        extend env p (Scheme (var_set, ty))
      in
      Base.List.fold2_exn
        (p1 :: p2 :: ps)
        (ty1 :: ty2 :: tys)
        ~f:helper
        ~init:(return env)
    | PAny, _ -> return env
    (* TODO : DEFINETELY DOES NOT WORK *)
    | PConstruct (name, patt_opt), Scheme (var_set, Tty_constr (param_tys, ident)) ->
      let* constr_entry, type_entry = find_constructor name env in
      if Ident.equal type_entry.tty_ident ident
      then (
        match patt_opt, constr_entry.constr_arg_ty with
        | None, None -> return env
        | Some actual_patt, Some expected_ty ->
          extend env actual_patt (Scheme (var_set, expected_ty))
        | _ -> fail (Constructor_arity_mismatch name))
      (* TODO : change message *)
      else fail Type_mismatch
    | _ -> fail (Not_implemented "extend")
  ;;
end

module Infer (M : Monads.STATE_MONAD) = struct
  module TypeEnv = TypeEnv (M)
  module Substitution = Substitution (M)
  module Scheme = Scheme (M)
  module State = State
  open State (M)
  open M

  let fresh_tvar_gen =
    let* state = get in
    let ident = Ident.ident state.cnt ("'_" ^ string_of_int state.cnt) in
    let* () = put { state with cnt = state.cnt + 1 } in
    return (Tty_var ident)
  ;;

  let instantiate (Scheme.Scheme (var_set, ty)) =
    let helper ident ty =
      let* ty = ty in
      let* tvar = fresh_tvar_gen in
      let* () = put init_state in
      let* () = Substitution.extend ident tvar in
      Substitution.apply ty
    in
    let* state = get in
    let* ty = VarSet.fold helper var_set (return ty) in
    let* () = put state in
    return ty
  ;;

  let generalize env ty =
    Scheme.Scheme (VarSet.diff (VarSet.free_vars ty) (TypeEnv.free_vars env), ty)
  ;;

  let instantiate_constr_params pty_params =
    let* tty_params =
      Base.List.fold
        ~f:(fun acc (ident : Ident.t) ->
          let* tys = acc in
          let* fresh = gen_fresh_int in
          let ty = Tty_var (Ident.ident fresh (Format.sprintf "'ty%d" fresh)) in
          let* () = Substitution.extend ident ty in
          return (ty :: tys))
        pty_params
        ~init:(return [])
    in
    return (List.rev tty_params)
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
    (* TODO : check if params instantiation works correctly *)
    (* TODO : TEST IT !!!! *)
    | PConstruct (name, patt_opt) ->
      let* constr_entry, type_entry = TypeEnv.find_constructor name env in
      (match constr_entry.constr_arg_ty, patt_opt with
       | None, None ->
         let* tys = instantiate_constr_params type_entry.tty_params in
         return (env, Tty_constr (tys, type_entry.tty_ident))
       | Some expected_ty, Some actual_patt ->
         let* env, actual_ty = pattern env actual_patt in
         let* () = Substitution.unify expected_ty actual_ty in
         let* param_tys = instantiate_constr_params type_entry.tty_params in
         let ty = Tty_constr (param_tys, type_entry.tty_ident) in
         let* ty = Substitution.apply ty in
         return (env, ty)
       | _ -> fail (Constructor_arity_mismatch name))
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
      (match Ident.Ident_map.find_by_name_opt name env.env_values with
       | Some (Typedtree.Scheme.Scheme (_, ty)) -> return ty
       | None -> fail (Unbound_value name))
    | EBinop (op, e1, e2) ->
      let* ty1 = expression env e1 in
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
      let* ty = Substitution.apply (Tty_prod (ty1, ty2, tys)) in
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
      let* subject_ty = expression env subject in
      let* env1, ty_p1 = pattern env p1 in
      let* ty_e1 = expression env1 e1 in
      let* ty_patt_cases, ty_expr_cases =
        Base.List.fold ~f:helper ~init:(return (ty_p1, ty_e1)) cases
      in
      let* () = Substitution.unify subject_ty ty_patt_cases in
      let* ty = Substitution.apply ty_expr_cases in
      return ty
    | EConstruct (name, arg_opt) ->
      let* constr_entry, type_declaration = TypeEnv.find_constructor name env in
      let tty_params = type_declaration.tty_params in
      let* tys = instantiate_constr_params tty_params in
      let ty = Tty_constr (tys, type_declaration.tty_ident) in
      (match constr_entry.constr_arg_ty, arg_opt with
       | Some expected_arg_ty, Some arg ->
         let* arg_ty = expression env arg in
         let* () = Substitution.unify expected_arg_ty arg_ty in
         let* ty = Substitution.apply ty in
         return ty
       | None, None -> return ty
       | _ -> fail (Constructor_arity_mismatch name))
    | _ -> fail (Not_implemented "infer expression")
  ;;

  let single_non_rec_vb env patt expr =
    let* _env, patt_ty = pattern env patt in
    let* expr_ty = expression env expr in
    let* () = Substitution.unify patt_ty expr_ty in
    let tvb_scheme = generalize env expr_ty in
    let* env = TypeEnv.extend env patt tvb_scheme in
    return (env, { tvb_flag = NonRecursive; tvb_pat = patt; tvb_body = expr; tvb_scheme })
  ;;

  let init_params pty_params =
    let helper acc name =
      let* env, tty_params = acc in
      let* fresh = gen_fresh_int in
      let ident = Ident.ident fresh name in
      let ty = Tty_var ident in
      let env = Ident.Ident_map.add ident ty env in
      return (env, ident :: tty_params)
    in
    let* env, tty_params =
      Base.List.fold ~init:(return (Ident.Ident_map.empty, [])) ~f:helper pty_params
    in
    return (env, List.rev tty_params)
  ;;

  let rec infer_core_type env param_map = function
    | Pty_var name ->
      (match Ident.Ident_map.find_by_name_opt name param_map with
       | None -> fail (Unbound_type_variable name)
       | Some ty -> return ty)
    | Pty_arrow (a, b) ->
      return (fun a b -> Tty_arrow (a, b))
      <*> infer_core_type env param_map a
      <*> infer_core_type env param_map b
    | Pty_tuple (a, b, xs) ->
      return (fun a b xs -> Tty_prod (a, b, xs))
      <*> infer_core_type env param_map a
      <*> infer_core_type env param_map b
      <*> fold_infer_core_type env param_map xs
    | Pty_constr (name, args) ->
      (match Ident.Ident_map.find_by_name_opt name env.env_types with
       | None -> fail (Type_was_not_declared name)
       | Some td ->
         if List.length args <> List.length td.tty_params
         then fail (Type_arity_mismatch name)
         else
           let* tys = fold_infer_core_type env param_map args in
           return (Tty_constr (tys, td.tty_ident)))

  and fold_infer_core_type env param_map items =
    Base.List.fold (List.rev items) ~init:(return []) ~f:(fun acc item ->
      let* acc = acc in
      let* ty = infer_core_type env param_map item in
      return (ty :: acc))

  and infer_core_type_opt env param_map = function
    | None -> return None
    | Some core_type ->
      let* ty = infer_core_type env param_map core_type in
      return (Some ty)
  ;;

  let check_params_uniqueness pty_name pty_params =
    if
      List.length pty_params
      <> List.length (Base.List.dedup_and_sort pty_params ~compare:String.compare)
    then fail (Type_param_duplicates pty_name)
    else return ()
  ;;

  let single_td env { pty_name; pty_params; pty_kind } =
    let* () = check_params_uniqueness pty_name pty_params in
    let* params_map, tty_params = init_params pty_params in
    let* fresh = gen_fresh_int in
    let tty_ident = Ident.ident fresh pty_name in
    match pty_kind with
    | Pty_abstract core_type_opt ->
      let* ty_opt = infer_core_type_opt env params_map core_type_opt in
      let tty_kind = Tty_abstract ty_opt in
      let td = { tty_kind; tty_ident; tty_params } in
      let env = TypeEnv.add_type td env in
      return (env, td)
    | Pty_variants (v1, vs) ->
      (* temporarily adds type to env to use it in recursive type declarations *)
      let env =
        TypeEnv.add_type { tty_kind = Tty_abstract None; tty_params; tty_ident } env
      in
      (* --- *)
      let infer_variant acc (name, core_type_opt) =
        let* env, acc = acc in
        let* fresh = gen_fresh_int in
        let ident = Ident.ident fresh name in
        let* ty_opt = infer_core_type_opt env params_map core_type_opt in
        let constr =
          { constr_arity = get_arity ty_opt
          ; constr_arg_ty = ty_opt
          ; constr_ident = ident
          ; constr_type_ident = tty_ident
          }
        in
        let env = TypeEnv.add_constructor constr env in
        return (env, (ident, ty_opt) :: acc)
      in
      let* env, variants =
        Base.List.fold ~init:(return (env, [])) ~f:infer_variant (v1 :: vs)
      in
      let tty_kind = Tty_variants (List.rev variants) in
      let td = { tty_kind; tty_ident; tty_params } in
      let env = TypeEnv.add_type td env in
      return (env, td)
  ;;

  let structure_item env = function
    | Pstr_value (NonRecursive, ((patt, expr), [])) ->
      let* env, vb = single_non_rec_vb env patt expr in
      return (env, Tstr_value vb)
    | Pstr_type (td, []) ->
      let* env, td = single_td env td in
      return (env, Tstr_type td)
    | _ -> fail (Not_implemented "structure item inferencer")
  ;;

  let structure env (item1, items) =
    let* env, typed_item1 = structure_item env item1 in
    let* _env, typed_items =
      Base.List.fold
        items
        ~init:(return (env, []))
        ~f:(fun acc item ->
          let* env, items = acc in
          let* env, new_item = structure_item env item in
          return (env, new_item :: items))
    in
    return (typed_item1, List.rev typed_items)
  ;;

  let expression ast =
    match M.run (expression TypeEnv.empty ast) State.init_state with
    | Ok (_state, ty) -> Ok ty
    | Error e -> Error e
  ;;

  let structure ast =
    match M.run (structure TypeEnv.empty ast) State.init_state with
    | Ok (_state, typed_stru) -> Ok typed_stru
    | Error e -> Error e
  ;;
end
