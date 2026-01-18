[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Parsetree
open Typedtree

module Error = struct
  type t =
    | Not_implemented of string
    | Occurs_check of string * ty
    | Unification_failed of ty * ty
    | Type_was_not_declared of string
    | Unbound_value of string
    | Unbound_type_variable of string
    | Unbound_constructor of string
    | Type_param_duplicates of string
    | Constructor_arity_mismatch of string
    | Left_of_let_rec
    | Right_of_let_rec
    | Constructor_name_duplicates of string
    | TypeEnv_invariant_violation of string

  let show = function
    | Not_implemented name -> Format.sprintf "not implemented: %s" name
    | Occurs_check (var, ty) -> Format.sprintf "%s occurs in %s" var (show_ty ty)
    | Unification_failed (a, b) ->
      Format.sprintf
        "unification of %s and %s failed"
        (Typedtree.show_ty a)
        (Typedtree.show_ty b)
    | Unbound_value name -> Format.sprintf "unbound value: %s" name
    | Type_was_not_declared name -> Format.sprintf "type %s was not declared" name
    | Unbound_type_variable name -> Format.sprintf "unbound type variable: %s" name
    | Type_param_duplicates name ->
      Format.sprintf "type param duplicates in declaration of type %s" name
    | Unbound_constructor name -> Format.sprintf "unbound constructor: %s" name
    | Constructor_arity_mismatch name ->
      Format.sprintf "constructor arity mismatch: %s" name
    | Left_of_let_rec -> "only variables are allowed as left-hand side of let rec"
    | Right_of_let_rec ->
      "this kind of expression is not allowed as right-hand side of let rec"
    | TypeEnv_invariant_violation msg ->
      Format.sprintf "type env invariant violation: %s" msg
    | Constructor_name_duplicates ty_name ->
      Format.sprintf "constructor name in declaration of type %s duplicates" ty_name
  ;;

  let pp ppf err = Format.fprintf ppf "%s" (show err)
end

open Error

module State (M : Monads.STATE_MONAD) = struct
  type t = { cnt : int }

  let empty = { cnt = 0 }

  open M

  let fresh_int =
    let* state = get in
    let rez = state.cnt in
    let* () = put { cnt = state.cnt + 1 } in
    return rez
  ;;

  let fresh_tvar =
    let* fresh_id = fresh_int in
    return (Tty_var (Ident.ident fresh_id (Format.sprintf "'ty%d" fresh_id)))
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
  open M
  open Base

  type t = (string, ty, String.comparator_witness) Map.t

  let empty = Map.empty (module String)
  let walk sub name = Map.find sub name

  let ident_equal (ident1 : Ident.t) (ident2 : Ident.t) =
    String.equal ident1.name ident2.name
  ;;

  let occurs_in name ty =
    let rec helper = function
      | Tty_var ident -> String.equal name ident.name
      | Tty_prod (ty1, ty2, tys) -> List.exists ~f:helper (ty1 :: ty2 :: tys)
      | Tty_arrow (ty1, ty2) -> helper ty1 || helper ty2
      | Tty_constr (tys, _ident) -> List.exists ~f:helper tys
    in
    match ty with
    | Tty_var _ -> false
    | _ -> helper ty
  ;;

  let occurs_check name ty =
    if occurs_in name ty then fail (Occurs_check (name, ty)) else return ()
  ;;

  let rec extend sub name ty =
    let* () = occurs_check name ty in
    match walk sub name with
    | Some old_ty ->
      let* sub2 = unify (apply sub ty) (apply sub old_ty) in
      compose sub sub2
    | None -> return (Map.set sub ~key:name ~data:(apply sub ty))

  and apply sub ty =
    match ty with
    | Tty_var ident ->
      (match walk sub ident.name with
       | Some new_ty -> apply sub new_ty
       | None -> Tty_var ident)
    | Tty_prod (ty1, ty2, tys) ->
      Tty_prod (apply sub ty1, apply sub ty2, apply_many sub tys)
    | Tty_arrow (ty1, ty2) -> Tty_arrow (apply sub ty1, apply sub ty2)
    | Tty_constr (tys, ident) -> Tty_constr (apply_many sub tys, ident)

  and apply_many sub tys = List.map tys ~f:(apply sub)

  and unify ty1 ty2 =
    match ty1, ty2 with
    | Tty_var ident1, Tty_var ident2 when ident_equal ident1 ident2 -> return empty
    | Tty_var ident, ty when occurs_in ident.name ty ->
      fail (Occurs_check (ident.name, ty2))
    | ty, Tty_var ident when occurs_in ident.name ty ->
      fail (Occurs_check (ident.name, ty2))
    | Tty_var ident, ty | ty, Tty_var ident ->
      return (Map.singleton (module String) ident.name ty)
    | Tty_prod (x1, x2, xs), Tty_prod (y1, y2, ys) when List.length xs = List.length ys ->
      unify_many_exn (x1 :: x2 :: xs) (y1 :: y2 :: ys)
    | Tty_arrow (x1, x2), Tty_arrow (y1, y2) -> unify_many_exn [ x1; x2 ] [ y1; y2 ]
    | Tty_constr (tys1, ident1), Tty_constr (tys2, ident2)
      when String.equal ident1 ident2 && List.length tys1 = List.length tys2 ->
      unify_many_exn tys1 tys2
    | _ -> fail (Unification_failed (ty1, ty2))

  and unify_many_exn tys1 tys2 =
    let unify_m acc ty1 ty2 =
      let* acc_sub = acc in
      let* new_sub = unify (apply acc_sub ty1) (apply acc_sub ty2) in
      compose acc_sub new_sub
    in
    List.fold2_exn ~init:(return empty) ~f:unify_m tys1 tys2

  and compose sub1 sub2 =
    let helper ~key ~data acc =
      let* acc_sub = acc in
      extend acc_sub key data
    in
    Map.fold ~init:(return sub1) ~f:helper sub2
  ;;

  let remove sub (ident : Ident.t) = Map.remove sub ident.name
end

module Scheme (M : Monads.STATE_MONAD) = struct
  include Typedtree.Scheme
  module Substitution = Substitution (M)

  let free_vars (Scheme.Scheme (varset, ty)) = VarSet.diff (VarSet.free_vars ty) varset

  let apply sub (Scheme.Scheme (bind_set, ty)) =
    let new_sub = VarSet.fold (fun key sub -> Substitution.remove sub key) bind_set sub in
    Scheme.Scheme (bind_set, Substitution.apply new_sub ty)
  ;;
end

module TypeEnv (M : Monads.STATE_MONAD) = struct
  open M
  open Base
  module Scheme = Scheme (M)
  module Substitution = Substitution (M)

  type t =
    { env_values : (string, Scheme.t, String.comparator_witness) Map.t
    ; env_types : (string, type_declaration, String.comparator_witness) Map.t
    ; env_constructors : (string, constructor_info, String.comparator_witness) Map.t
    }

  let empty =
    { env_values = Map.empty (module String)
    ; env_types = Map.empty (module String)
    ; env_constructors = Map.empty (module String)
    }
  ;;

  let typ_unit : type_declaration =
    { tty_ident = Ident.ident (-1) "unit"; tty_params = []; tty_kind = Tty_abstract None }
  ;;

  let typ_int : type_declaration =
    { tty_ident = Ident.ident (-2) "int"; tty_params = []; tty_kind = Tty_abstract None }
  ;;

  let typ_bool : type_declaration =
    { tty_ident = Ident.ident (-3) "bool"; tty_params = []; tty_kind = Tty_abstract None }
  ;;

  module TypeList : sig
    val typ_list : type_declaration
    val constr_nil : constructor_info
    val constr_cons : constructor_info
  end = struct
    let param_ident = Ident.ident (-1) "'a"
    let param_ty = Tty_var param_ident
    let nil_ident = Ident.ident 0 "[]"
    let cons_ident = Ident.ident 1 "::"
    let cons_arg_ty = Some (Tty_prod (param_ty, Tty_constr ([ param_ty ], "list"), []))

    let typ_list : type_declaration =
      { tty_ident = Ident.ident (-4) "list"
      ; tty_params = [ param_ident ]
      ; tty_kind = Tty_variants [ nil_ident, None; cons_ident, cons_arg_ty ]
      }
    ;;

    let constr_nil : constructor_info =
      { constr_arg = None
      ; constr_type_ident = typ_list.tty_ident
      ; constr_ident = nil_ident
      }
    ;;

    let constr_cons : constructor_info =
      { constr_arg = cons_arg_ty
      ; constr_type_ident = typ_list.tty_ident
      ; constr_ident = cons_ident
      }
    ;;
  end

  let fix_scheme =
    let a_ident = Ident.ident (-5) "'a" in
    let a_var = tty_var a_ident in
    let fix_ty = tty_arrow (tty_arrow a_var a_var) a_var in
    Scheme.Scheme (VarSet.singleton a_ident, fix_ty)
  ;;

  let set_type (td : type_declaration) (env : t) =
    let key = td.tty_ident.name in
    { env with env_types = Map.set ~key ~data:td env.env_types }
  ;;

  let set_constructor (constr : constructor_info) (env : t) =
    let key = constr.constr_ident.name in
    { env with env_constructors = Map.set ~key ~data:constr env.env_constructors }
  ;;

  let set_value ~name (scheme : Scheme.t) (env : t) =
    { env with env_values = Map.set ~key:name ~data:scheme env.env_values }
  ;;

  let env_with_base_types =
    empty
    |> set_type typ_unit
    |> set_type typ_int
    |> set_type typ_bool
    |> set_type TypeList.typ_list
    |> set_constructor TypeList.constr_nil
    |> set_constructor TypeList.constr_cons
    |> set_value ~name:"fix" fix_scheme
  ;;

  let free_vars (env : t) =
    Map.fold env.env_values ~init:VarSet.empty ~f:(fun ~key:_ ~data acc ->
      VarSet.union acc (Scheme.free_vars data))
  ;;

  let find (env : t) = Map.find env.env_values

  let rec extend env pat (Scheme.Scheme (varset, ty) as scheme) =
    match pat, ty with
    | PAny, _ -> return env
    | PVar name, _ -> return (set_value ~name scheme env)
    | PTuple (p1, p2, ps), Tty_prod (ty1, ty2, tys) when List.length ps = List.length tys
      -> extend_many_exn env varset (p1 :: p2 :: ps) (ty1 :: ty2 :: tys)
    | PConstant CUnit, Tty_constr ([], "unit")
    | PConstant (CInt _), Tty_constr ([], "int")
    | PConstant (CBool _), Tty_constr ([], "bool") -> return env
    | PConstruct _, _ ->
      fail (Not_implemented "pconstruct in extend env (use match-with idk)")
    | _ -> fail (TypeEnv_invariant_violation "scheme does not correspond pattern")

  and extend_many_exn env varset patts tys =
    let helper acc patt ty =
      let* env = acc in
      extend env patt (Scheme (varset, ty))
    in
    List.fold2_exn ~f:helper patts tys ~init:(return env)
  ;;

  let extend_types ~ident tty_params tty_kind (env : t) =
    let type_entry = { tty_ident = ident; tty_params; tty_kind } in
    { env with env_types = Base.Map.set ~key:ident.name ~data:type_entry env.env_types }
  ;;

  let apply_to_type_declaration sub { tty_ident; tty_params; tty_kind } =
    let aux sub =
      let sub = List.fold ~f:Substitution.remove tty_params ~init:sub in
      Option.map ~f:(Substitution.apply sub)
    in
    { tty_ident
    ; tty_params
    ; tty_kind =
        (match tty_kind with
         | Tty_abstract ty_opt -> Tty_abstract (aux sub ty_opt)
         | Tty_variants vs ->
           Tty_variants (List.map vs ~f:(fun (name, ty_opt) -> name, aux sub ty_opt)))
    }
  ;;

  let apply_to_constructor_info sub { constr_ident; constr_arg; constr_type_ident } =
    { constr_arg = Option.map ~f:(Substitution.apply sub) constr_arg
    ; constr_type_ident
    ; constr_ident
    }
  ;;

  let apply sub (env : t) =
    let env_values = Base.Map.map env.env_values ~f:(Scheme.apply sub) in
    let env_types = Base.Map.map env.env_types ~f:(apply_to_type_declaration sub) in
    let env_constructors =
      Base.Map.map env.env_constructors ~f:(apply_to_constructor_info sub)
    in
    { env_values; env_types; env_constructors }
  ;;
end

module Infer (M : Monads.STATE_MONAD) = struct
  open M
  open Base
  module Substitution = Substitution (M)
  module State = State (M)
  module Scheme = Scheme (M)
  module TypeEnv = TypeEnv (M)

  let instantiate (Scheme.Scheme (bind_set, ty)) =
    VarSet.fold
      (fun ident ty ->
         let* ty = ty in
         let* fresh = State.fresh_tvar in
         let sub = Map.singleton (module String) ident.name fresh in
         return (Substitution.apply sub ty))
      bind_set
      (return ty)
  ;;

  let generalize (env : TypeEnv.t) ty =
    Scheme.Scheme (VarSet.diff (VarSet.free_vars ty) (TypeEnv.free_vars env), ty)
  ;;

  let apply, unify, compose = Substitution.(apply, unify, compose)

  let unify2 acc_env acc_sub ty1 ty2 =
    let ty1 = apply acc_sub ty1 in
    let ty2 = apply acc_sub ty2 in
    let* new_sub = unify ty1 ty2 in
    let* sub = compose acc_sub new_sub in
    return (TypeEnv.apply sub acc_env, sub)
  ;;

  let find_constructor name (env : TypeEnv.t) =
    match Map.find env.env_constructors name with
    | None -> fail (Unbound_constructor name)
    | Some constr_entry ->
      let ty_ident = constr_entry.constr_type_ident in
      (match Map.find env.env_types ty_ident.name with
       | None ->
         fail (TypeEnv_invariant_violation "constructor references a non-existent type")
       | Some type_entry -> return (constr_entry, type_entry))
  ;;

  let make_freshes n =
    let rec aux n acc =
      if n < 1
      then return (List.rev acc)
      else
        let* fresh = State.fresh_tvar in
        aux (n - 1) (fresh :: acc)
    in
    aux n []
  ;;

  let instantiate_adt type_info =
    let* fresh_tys = make_freshes (List.length type_info.tty_params) in
    let sub =
      List.fold2_exn
        type_info.tty_params
        fresh_tys
        ~f:(fun map id ty -> Base.Map.set ~key:id.name ~data:ty map)
        ~init:(Base.Map.empty (module String))
    in
    let ty = Tty_constr (fresh_tys, type_info.tty_ident.name) in
    return (sub, ty)
  ;;

  let constant = function
    | CUnit -> ty_unit
    | CInt _ -> ty_int
    | CBool _ -> ty_bool
  ;;

  let rec pattern env = function
    | PAny ->
      let* ty = State.fresh_tvar in
      return (env, ty)
    | PVar name ->
      let* fresh = State.fresh_tvar in
      let scheme = Scheme.Scheme (VarSet.empty, fresh) in
      let env = TypeEnv.set_value ~name scheme env in
      return (env, fresh)
    | PConstant x -> return (env, constant x)
    | PTuple (p1, p2, ps) ->
      let* env, ty1 = pattern env p1 in
      let* env, ty2 = pattern env p2 in
      let* env, tys = pattern_many env ps in
      return (env, Tty_prod (ty1, ty2, tys))
    | PConstruct (name, patt_opt) ->
      let* constr_info, type_info = find_constructor name env in
      let* sub, ty = instantiate_adt type_info in
      let arg_ty_opt = Option.map ~f:(apply sub) constr_info.constr_arg in
      (match arg_ty_opt, patt_opt with
       | None, None -> return (env, ty)
       | Some arg_ty, Some patt ->
         let* env, patt_ty = pattern env patt in
         let* sub = unify patt_ty arg_ty in
         let env = TypeEnv.apply sub env in
         let ty = apply sub ty in
         return (env, ty)
       | _ -> fail (Constructor_arity_mismatch name))

  and pattern_many env ps =
    let helper acc patt =
      let* env, tys = acc in
      let* env, ty = pattern env patt in
      return (env, ty :: tys)
    in
    let* env, tys = Base.List.fold ps ~init:(return (env, [])) ~f:helper in
    return (env, List.rev tys)
  ;;

  let type_of_binop = function
    | Add | Sub | Mul | Div -> return (ty_int, ty_int, ty_int)
    | Eq | Ne | Le | Ge | Lt | Gt -> return (ty_int, ty_int, ty_bool)
  ;;

  let rec expression acc_env acc_sub expr =
    let helper env sub = function
      | EVar name ->
        (match TypeEnv.find env name with
         | Some scheme ->
           let* ty = instantiate (Scheme.apply sub scheme) in
           return (env, sub, ty)
         | None -> fail (Unbound_value name))
      | EConstant x -> return (env, sub, constant x)
      | ETuple (e1, e2, es) ->
        let* env, sub, ty1 = expression env sub e1 in
        let* env, sub, ty2 = expression env sub e2 in
        let* env, sub, tys = expression_many env sub es in
        return (env, sub, Tty_prod (ty1, ty2, tys))
      | EApp (e1, e2) ->
        let* env, sub, ty1 = expression env sub e1 in
        let* env, sub, ty2 = expression env sub e2 in
        let* ty = State.fresh_tvar in
        let* env, sub = unify2 env sub ty1 (Tty_arrow (ty2, ty)) in
        return (env, sub, ty)
      | EFun (patt, expr) ->
        let* patt_env, patt_ty = pattern env patt in
        let* _env, sub, expr_ty = expression patt_env sub expr in
        return (env, sub, Tty_arrow (patt_ty, expr_ty))
      | EBinop (op, e1, e2) ->
        let* env, sub, ty1 = expression env sub e1 in
        let* env, sub, ty2 = expression env sub e2 in
        let* ty1_expected, ty2_expected, ty = type_of_binop op in
        let* env, sub = unify2 env sub ty1 ty1_expected in
        let* env, sub = unify2 env sub ty2 ty2_expected in
        return (env, sub, ty)
      | EIf (expr_cond, expr_then, expr_else) ->
        let* env, sub, ty_cond = expression env sub expr_cond in
        let* env, sub = unify2 env sub ty_cond ty_bool in
        let* env, sub, ty_then = expression env sub expr_then in
        let* env, sub, ty_else = expression env sub expr_else in
        let* env, sub = unify2 env sub ty_then ty_else in
        return (env, sub, ty_then)
      | EMatch (subject, ((p1, e1), cases)) ->
        let infer_case acc (patt, expr) =
          let* env, sub, acc_patt_ty, acc_expr_ty = acc in
          let* patt_env, patt_ty = pattern env patt in
          let* env, sub = unify2 env sub acc_patt_ty patt_ty in
          let* _env, sub, expr_ty = expression patt_env sub expr in
          let* env, sub = unify2 env sub acc_expr_ty expr_ty in
          return (env, sub, apply sub patt_ty, apply sub expr_ty)
        in
        let* env, sub, subject_ty = expression env sub subject in
        let* p1_env, p1_ty = pattern env p1 in
        let* env, sub = unify2 env sub subject_ty p1_ty in
        let* _env, sub, e1_ty = expression p1_env sub e1 in
        let* env, sub, _pty, ety =
          List.fold ~f:infer_case ~init:(return (env, sub, p1_ty, e1_ty)) cases
        in
        return (env, sub, ety)
      | ELet (rec_flag, (vb1, vbs), body) ->
        let infer_vb =
          match rec_flag with
          | Recursive -> rec_value_binding
          | NonRecursive -> value_binding
        in
        let helper acc (patt, expr) =
          let* env, sub = acc in
          let* env, sub, _scheme = infer_vb env sub (patt, expr) in
          return (env, sub)
        in
        let* inner_env, sub =
          List.fold ~f:helper (vb1 :: vbs) ~init:(return (env, sub))
        in
        let* _env, sub, ty = expression inner_env sub body in
        return (env, sub, ty)
      | EConstruct (name, expr_opt) ->
        let* constr_info, type_info = find_constructor name env in
        let* inst_sub, ty = instantiate_adt type_info in
        (match constr_info.constr_arg, expr_opt with
         | None, None -> return (env, sub, ty)
         | Some arg_ty, Some expr ->
           let arg_ty = Substitution.apply inst_sub arg_ty in
           let* env, sub, expr_ty = expression env sub expr in
           let* env, sub = unify2 env sub arg_ty expr_ty in
           return (env, sub, ty)
         | _ -> fail (Constructor_arity_mismatch name))
    in
    let acc_env = TypeEnv.apply acc_sub acc_env in
    let* expr_env, expr_sub, expr_ty = helper acc_env acc_sub expr in
    let* sub = Substitution.compose acc_sub expr_sub in
    let env = TypeEnv.apply sub expr_env in
    let ty = Substitution.apply sub expr_ty in
    return (env, sub, ty)

  and expression_many env sub exprs =
    let helper acc expr =
      let* env, sub, tys = acc in
      let* env, sub, ty = expression env sub expr in
      return (env, sub, ty :: tys)
    in
    let* env, sub, tys = List.fold ~f:helper ~init:(return (env, sub, [])) exprs in
    return (env, sub, List.rev (List.map ~f:(apply sub) tys))

  and value_binding env sub (patt, expr) =
    let* env, sub, expr_ty = expression env sub expr in
    let scheme = generalize env expr_ty in
    let* patt_env, patt_ty = pattern env patt in
    let* env = TypeEnv.extend patt_env patt scheme in
    let* env, sub = unify2 env sub patt_ty expr_ty in
    return (env, sub, scheme)

  and rec_value_binding env sub (patt, expr) =
    match patt with
    | PVar _ ->
      let* env, patt_ty = pattern env patt in
      let* env = TypeEnv.extend env patt (Scheme.Scheme (VarSet.empty, patt_ty)) in
      let* env, sub, expr_ty = expression env sub expr in
      let scheme = generalize env expr_ty in
      (match expr_ty with
       | Tty_arrow _ as arrow ->
         let* env, sub = unify2 env sub patt_ty arrow in
         return (env, sub, Scheme.apply sub scheme)
       | _ -> fail Right_of_let_rec)
    | _ -> fail Left_of_let_rec
  ;;

  let tstr_value ~rec_flag env ((patt1, expr1), vbs) =
    let make_vb patt expr scheme =
      { tvb_pat = patt; tvb_body = expr; tvb_scheme = scheme }
    in
    let infer_vb =
      match rec_flag with
      | Recursive -> rec_value_binding
      | NonRecursive -> value_binding
    in
    let helper acc (patt, expr) =
      let* acc_env, acc_sub, acc_items = acc in
      let* env, sub, scheme = infer_vb acc_env acc_sub (patt, expr) in
      let new_item = make_vb patt expr scheme in
      return (env, sub, new_item :: acc_items)
    in
    let* env, sub, scheme1 = infer_vb env Substitution.empty (patt1, expr1) in
    let* env, _sub, stru_items = List.fold ~f:helper ~init:(return (env, sub, [])) vbs in
    let stru_item1 = make_vb patt1 expr1 scheme1 in
    return (env, Tstr_value (rec_flag, stru_item1, List.rev stru_items))
  ;;

  let check_constr_arity (env : TypeEnv.t) name args =
    match Base.Map.find env.env_types name with
    | None -> fail (Type_was_not_declared name)
    | Some type_declaration ->
      if List.length args <> List.length type_declaration.tty_params
      then fail (Constructor_arity_mismatch name)
      else return ()
  ;;

  let rec infer_core_type (env : TypeEnv.t) param_map = function
    | Parsetree.Pty_var name ->
      (match Base.Map.find param_map name with
       | None -> fail (Unbound_type_variable name)
       | Some ty -> return ty)
    | Pty_arrow (a, b) ->
      return (fun a b -> tty_arrow a b)
      <*> infer_core_type env param_map a
      <*> infer_core_type env param_map b
    | Pty_tuple (a, b, xs) ->
      return (fun a b xs -> tty_prod a b xs)
      <*> infer_core_type env param_map a
      <*> infer_core_type env param_map b
      <*> fold_infer_core_type env param_map xs
    | Pty_constr (name, args) ->
      let* () = check_constr_arity env name args in
      let* tys = fold_infer_core_type env param_map args in
      return (tty_constr tys name)

  and fold_infer_core_type (env : TypeEnv.t) param_map items =
    List.fold (List.rev items) ~init:(return []) ~f:(fun acc item ->
      let* acc = acc in
      let* ty = infer_core_type env param_map item in
      return (ty :: acc))

  and infer_core_type_opt (env : TypeEnv.t) param_map = function
    | None -> return None
    | Some core_type ->
      let* ty = infer_core_type env param_map core_type in
      return (Some ty)
  ;;

  let str_ls_unique ls =
    List.length (List.dedup_and_sort ls ~compare:String.compare) = List.length ls
  ;;

  let check_params_uniqueness pty_name pty_params =
    if str_ls_unique pty_params then return () else fail (Type_param_duplicates pty_name)
  ;;

  let check_constructors_names_uniqueness pty_name variants =
    let names = List.map ~f:(fun (name, _) -> name) variants in
    if str_ls_unique names then return () else fail (Constructor_name_duplicates pty_name)
  ;;

  let type_declaration env { Parsetree.pty_name; pty_params; pty_kind } =
    let init_params params_list =
      let helper acc name =
        let* map, vars = acc in
        let* fresh = State.fresh_int in
        let ident = Ident.of_int fresh in
        let ty = tty_var ident in
        let map = Base.Map.set ~key:name ~data:ty map in
        return (map, vars @ [ ident ])
      in
      List.fold ~init:(return (Base.Map.empty (module String), [])) ~f:helper params_list
    in
    let* () = check_params_uniqueness pty_name pty_params in
    let* params_map, tty_params = init_params pty_params in
    let* fresh = State.fresh_int in
    let tty_ident = Ident.ident fresh pty_name in
    let extend_types = TypeEnv.extend_types ~ident:tty_ident tty_params in
    match pty_kind with
    | Pty_abstract core_type_opt ->
      let* ty_opt = infer_core_type_opt env params_map core_type_opt in
      let tty_kind = Tty_abstract ty_opt in
      let env = extend_types tty_kind env in
      let tstr_type = Tstr_type { tty_kind; tty_ident; tty_params } in
      return (env, tstr_type)
    | Pty_variants (v1, vs) ->
      let* () = check_constructors_names_uniqueness pty_name (v1 :: vs) in
      let env = extend_types (Tty_abstract None) env in
      let aux acc (name, core_type_opt) =
        let* env, variants, id_cnt = acc in
        let* constr_arg = infer_core_type_opt env params_map core_type_opt in
        let constr_ident = Ident.ident id_cnt name in
        let constructor_info =
          { constr_ident; constr_type_ident = tty_ident; constr_arg }
        in
        let env = TypeEnv.set_constructor constructor_info env in
        return (env, (constr_ident, constr_arg) :: variants, id_cnt + 1)
      in
      let* env, variants, _ = List.fold ~init:(return (env, [], 0)) ~f:aux (v1 :: vs) in
      let tty_kind = Tty_variants (List.rev variants) in
      let env = extend_types tty_kind env in
      let tstr_type = Tstr_type { tty_kind; tty_ident; tty_params } in
      return (env, tstr_type)
  ;;

  let structure_item env = function
    | Pstr_value (rec_flag, vbs) -> tstr_value ~rec_flag env vbs
    | Pstr_type (td, _tds) -> type_declaration env td
  ;;

  let structure env (item, items) =
    let helper acc item =
      let* acc_env, acc_items = acc in
      let* new_env, new_item = structure_item acc_env item in
      return (new_env, new_item :: acc_items)
    in
    let* env, typed_item = structure_item env item in
    let* _env, typed_items = List.fold ~f:helper ~init:(return (env, [])) items in
    return (typed_item, List.rev typed_items)
  ;;
end

module Make (M : Monads.STATE_MONAD) = struct
  module TypeEnv = TypeEnv (M)
  module Substitution = Substitution (M)
  module Infer = Infer (M)
  module State = State (M)

  let infer_expression ast =
    let rez = Infer.expression TypeEnv.empty Substitution.empty ast in
    match M.run rez State.empty with
    | Ok (_state, (_env, _sub, ty)) -> Ok ty
    | Error e -> Error e
  ;;

  let infer_structure ast =
    match M.run (Infer.structure TypeEnv.env_with_base_types ast) State.empty with
    | Ok (_state, typed_stru) -> Ok typed_stru
    | Error e -> Error e
  ;;
end
