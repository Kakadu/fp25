open Parsetree
open Typedtree

type error =
  | Not_implemented of string
  | Occurs_check of ty * ty
  | Unification_failed of ty * ty
  | Type_mismatch
  | Unbound_value of string
  | Type_was_not_declared of string
  | Type_arity_mismatch of string
  | Unbound_type_variable of string
  | Type_param_duplicates of string
  | Unbound_constructor of string
  | Type_env_invariant_violation of string
  | Constructor_arity_mismatch of string
  | Only_variables_on_the_left_of_let_rec
  | Only_functions_on_the_right_of_let_rec

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
  | Type_env_invariant_violation msg ->
    Format.sprintf "type environment invariant violation: %s" msg
  | Constructor_arity_mismatch name ->
    Format.sprintf "constructor arity mismatch: %s" name
  | Only_variables_on_the_left_of_let_rec ->
    "only variables are allowed as left-hand side of let rec"
  | Only_functions_on_the_right_of_let_rec ->
    "this kind of expression is not allowed as right-hand side of let rec"
;;

let pp_error ppf err = Format.fprintf ppf "%s" (show_error err)
let _debug = ref false

module State (M : Monads.STATE_MONAD) = struct
  type t = { cnt : int }

  let empty_state = { cnt = 0 }

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
  let walk_opt sub name = Map.find sub name

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

  let update sub name ty = Map.update sub name ~f:(fun _ -> ty)

  let occurs_check name ty =
    if occurs_in name ty then failwith "TODO : occurs check" else return ()
  ;;

  let rec extend sub name ty =
    let* () = occurs_check name ty in
    match walk_opt sub name with
    | Some old_ty ->
      let* sub2 = unify (apply sub ty) (apply sub old_ty) in
      compose sub sub2
    | None ->
      let init = Map.singleton (module String) name (apply sub ty) in
      let helper ~key ~data acc =
        let* acc = acc in
        return (update acc key (apply init data))
      in
      Map.fold sub ~init:(return init) ~f:helper

  and apply sub = function
    | Tty_var ident ->
      (match walk_opt sub ident.name with
       | Some ty -> apply sub ty
       | None -> Tty_var ident)
    | Tty_prod (ty1, ty2, tys) ->
      Tty_prod (apply sub ty1, apply sub ty2, apply_many sub tys)
    | Tty_arrow (ty1, ty2) -> Tty_arrow (apply sub ty1, apply sub ty2)
    | Tty_constr (tys, ident) -> Tty_constr (apply_many sub tys, ident)

  and apply_many sub tys = List.map tys ~f:(apply sub)

  and unify ty1 ty2 =
    match ty1, ty2 with
    | Tty_var ident1, Tty_var ident2 when ident_equal ident1 ident2 -> return empty
    | Tty_var ident, ty when occurs_in ident.name ty -> fail (Occurs_check (ty1, ty2))
    | ty, Tty_var ident when occurs_in ident.name ty -> fail (Occurs_check (ty1, ty2))
    | Tty_var ident, ty | ty, Tty_var ident ->
      return (Map.singleton (module String) ident.name ty)
    | Tty_prod (x1, x2, xs), Tty_prod (y1, y2, ys) when List.length xs = List.length ys ->
      unify_many_exn (x1 :: x2 :: xs) (y1 :: y2 :: ys)
    | Tty_arrow (x1, x2), Tty_arrow (y1, y2) -> unify_many_exn [ x1; x2 ] [ y1; y2 ]
    | Tty_constr (tys1, ident1), Tty_constr (tys2, ident2)
      when ident_equal ident1 ident2 && List.length tys1 = List.length tys2 ->
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

  and compose_many subs =
    let helper acc sub =
      let* acc_sub = acc in
      compose acc_sub sub
    in
    List.fold ~init:(return empty) ~f:helper subs
  ;;

  let remove sub (ident : Ident.t) = Map.remove sub ident.name

  (* debugging stuff *)
  let pp ppf (sub : t) =
    Format.fprintf ppf "{ ";
    Map.iteri sub ~f:(fun ~key ~data ->
      Format.fprintf ppf "[ %s -> %s ]  " key (show_ty data));
    Format.fprintf ppf " }"
  ;;
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
    ; env_constructors :
        (string, TypeEnv.constructor_entry, String.comparator_witness) Map.t
    }

  let empty =
    { env_values = Map.empty (module String)
    ; env_types = Map.empty (module String)
    ; env_constructors = Map.empty (module String)
    }
  ;;

  let update env key value =
    { env with env_values = Map.update env.env_values key ~f:(fun _ -> value) }
  ;;

  let free_vars env =
    Map.fold env.env_values ~init:VarSet.empty ~f:(fun ~key:_ ~data acc ->
      VarSet.union acc (Scheme.free_vars data))
  ;;

  let apply (sub : Substitution.t) env =
    { env with env_values = Map.map env.env_values ~f:(Scheme.apply sub) }
  ;;

  let find env = Map.find env.env_values

  let rec extend env pat (Scheme.Scheme (varset, ty) as scheme) =
    match pat, ty with
    | PAny, _ -> return env
    | PVar name, _ -> return (update env name scheme)
    | PTuple (p1, p2, ps), Tty_prod (ty1, ty2, tys) when List.length ps == List.length tys
      -> extend_many_exn env varset (p1 :: p2 :: ps) (ty1 :: ty2 :: tys)
    | PConstruct _, _ -> failwith "not implemented"
    | _ -> fail (Type_env_invariant_violation "scheme does not correspond pattern")

  and extend_many_exn env varset patts tys =
    let helper acc patt ty =
      let* env = acc in
      extend env patt (Scheme (varset, ty))
    in
    List.fold2_exn ~f:helper patts tys ~init:(return env)
  ;;

  let find_type_opt (ident : Ident.t) env = Map.find env.env_types ident.name

  let check_type_not_declared td env =
    match find_type_opt td.tty_ident env with
    | Some _ -> failwith "type already declared in scope"
    | None -> ()
  ;;

  let add_type td env =
    let () = check_type_not_declared td env in
    { env with env_types = Map.add_exn ~key:td.tty_ident.name ~data:td env.env_types }
  ;;

  let unit_ident = Ident.ident (-1) "unit"
  let int_ident = Ident.ident (-2) "int"
  let bool_ident = Ident.ident (-3) "bool"
  let ty_unit = Tty_constr ([], unit_ident)
  let ty_int = Tty_constr ([], int_ident)
  let ty_bool = Tty_constr ([], bool_ident)

  let base_types_env =
    empty
    |> add_type { tty_kind = Tty_abstract None; tty_ident = unit_ident; tty_params = [] }
    |> add_type { tty_kind = Tty_abstract None; tty_ident = int_ident; tty_params = [] }
    |> add_type { tty_kind = Tty_abstract None; tty_ident = bool_ident; tty_params = [] }
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

  let rec pattern env = function
    | PAny ->
      let* ty = State.fresh_tvar in
      return (env, ty)
    | PVar name ->
      let* fresh_type_var = State.fresh_tvar in
      if !_debug then Format.printf "<< fresh generated for pattern { %s } >>@." name;
      let env = TypeEnv.update env name (Scheme.Scheme (VarSet.empty, fresh_type_var)) in
      return (env, fresh_type_var)
    | PTuple (p1, p2, ps) ->
      let* env, ty1 = pattern env p1 in
      let* env, ty2 = pattern env p2 in
      let* env, tys = pattern_many env ps in
      return (env, Tty_prod (ty1, ty2, tys))
    | _ -> fail (Not_implemented "constructor pattern")

  and pattern_many env ps =
    let helper acc patt =
      let* env, tys = acc in
      let* env, ty = pattern env patt in
      return (env, ty :: tys)
    in
    let* env, tys = Base.List.fold ps ~init:(return (env, [])) ~f:helper in
    return (env, List.rev tys)
  ;;

  let constant = function
    | CUnit -> return (Substitution.empty, TypeEnv.ty_unit)
    | CInt _ -> return (Substitution.empty, TypeEnv.ty_int)
    | CBool _ -> return (Substitution.empty, TypeEnv.ty_bool)
  ;;

  let type_of_binop = function
    | Add | Sub | Mul | Div -> return (TypeEnv.ty_int, TypeEnv.ty_int, TypeEnv.ty_int)
    | Eq | Ne | Le | Ge | Lt | Gt ->
      return (TypeEnv.ty_bool, TypeEnv.ty_int, TypeEnv.ty_int)
    | _ -> fail (Not_implemented "type of binop")
  ;;

  let apply, unify, compose, compose_many =
    Substitution.(apply, unify, compose, compose_many)
  ;;

  let unify2 acc_env acc_sub ty1 ty2 =
    let ty1 = apply acc_sub ty1 in
    let ty2 = apply acc_sub ty2 in
    if !_debug
    then Format.printf "<< unify { %s } and { %s } >>@." (show_ty ty1) (show_ty ty2);
    let* new_sub = unify ty1 ty2 in
    let* sub = compose acc_sub new_sub in
    if !_debug
    then (
      Format.printf "<< (unify) old sub: %a >>@." Substitution.pp acc_sub;
      Format.printf "<< (unify) new sub: %a >>@." Substitution.pp sub);
    return (TypeEnv.apply sub acc_env, sub)
  ;;

  let constant = function
    | CUnit -> TypeEnv.ty_unit
    | CInt _ -> TypeEnv.ty_int
    | CBool _ -> TypeEnv.ty_bool
  ;;

  let rec expression acc_env acc_sub expr =
    if !_debug
    then
      Format.printf
        "<< expression called on { %s } with sub %a >>@."
        (show_expression expr)
        Substitution.pp
        acc_sub;
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
        if !_debug then Format.printf "<< unify called from EApp >>@.";
        let* env, sub = unify2 env sub ty1 (Tty_arrow (ty2, ty)) in
        return (env, sub, ty)
      | EFun (patt, expr) ->
        let* patt_env, patt_ty = pattern env patt in
        let* _env, sub, expr_ty = expression patt_env sub expr in
        return (env, sub, Tty_arrow (patt_ty, expr_ty))
      | EBinop (op, e1, e2) ->
        let* env, sub, ty1 = expression env sub e1 in
        let* env, sub, ty2 = expression env sub e2 in
        let* ty, ty1_expected, ty2_expected = type_of_binop op in
        if !_debug
        then
          Format.printf
            "<< unify called from EBinop on { %s } and { %s } >>@."
            (show_expression e1)
            (show_ty ty1_expected);
        let* env, sub = unify2 env sub ty1 ty1_expected in
        if !_debug
        then
          Format.printf
            "<< unify called from EBinop on { %s } and { %s } >>@."
            (show_expression e2)
            (show_ty ty2_expected);
        let* env, sub = unify2 env sub ty2 ty2_expected in
        return (env, sub, ty)
      | EIf (expr_cond, expr_then, expr_else) ->
        let* env, sub, ty_cond = expression env sub expr_cond in
        if !_debug
        then
          Format.printf
            "<< unify called from EIf on { %s } and { bool } >>@."
            (show_expression expr_cond);
        let* env, sub = unify2 env sub ty_cond TypeEnv.ty_bool in
        let* env, sub, ty_then = expression env sub expr_then in
        let* env, sub, ty_else = expression env sub expr_else in
        if !_debug
        then
          Format.printf
            "<< unify called from EIf on { %s } and { %s } >>@."
            (show_expression expr_then)
            (show_expression expr_else);
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
      | ELet (NonRecursive, (vb1, vbs), body) ->
        let helper acc (patt, expr) =
          let* env, sub = acc in
          let* env, sub, _scheme = value_binding env sub (patt, expr) in
          return (env, sub)
        in
        let* inner_env, sub =
          List.fold ~f:helper (vb1 :: vbs) ~init:(return (env, sub))
        in
        let* _env, sub, ty = expression inner_env sub body in
        return (env, sub, ty)
      | _ -> fail (Not_implemented "expression")
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
  ;;

  let tstr_non_rec_value env ((patt1, expr1), vbs) =
    let non_rec_vb patt expr scheme =
      { tvb_flag = NonRecursive; tvb_pat = patt; tvb_body = expr; tvb_scheme = scheme }
    in
    let helper acc (patt, expr) =
      let* acc_env, acc_sub, acc_items = acc in
      let* env, sub, scheme = value_binding acc_env acc_sub (patt, expr) in
      let new_item = non_rec_vb patt expr scheme in
      return (env, sub, new_item :: acc_items)
    in
    let* env, sub, scheme1 = value_binding env Substitution.empty (patt1, expr1) in
    let* env, _sub, stru_items = List.fold ~f:helper ~init:(return (env, sub, [])) vbs in
    let stru_item1 = non_rec_vb patt1 expr1 scheme1 in
    return (env, Tstr_value (stru_item1, List.rev stru_items))
  ;;

  let expression_main ast =
    let* _env, _sub, ty = expression TypeEnv.empty Substitution.empty ast in
    return ty
  ;;

  let expression ?(debug = false) ast =
    _debug := debug;
    match M.run (expression_main ast) State.empty_state with
    | Ok (_state, ty) -> Ok ty
    | Error e -> Error e
  ;;

  let structure_item env = function
    | Pstr_value (NonRecursive, vbs) -> tstr_non_rec_value env vbs
    | _ -> fail (Not_implemented "stru item")
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

  let structure ?(debug = false) ast =
    _debug := debug;
    match M.run (structure TypeEnv.empty ast) State.empty_state with
    | Ok (_state, typed_stru) -> Ok typed_stru
    | Error e -> Error e
  ;;
end
