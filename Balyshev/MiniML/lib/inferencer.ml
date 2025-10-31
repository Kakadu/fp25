open Ast

type error =
  | Not_implemented of string
  | Occurs_check of string * ty
  | Unification_failed of ty * ty
  | Type_mismatch
  | Unbound_value of string

let show_error = function
  | Not_implemented name -> Format.sprintf "not implemented: %s" name
  | Occurs_check (name, ty) -> Format.sprintf "%s occurs in %s" name (show_ty ty)
  | Unification_failed (a, b) ->
    Format.sprintf "unification of %s and %s failed" (show_ty a) (show_ty b)
  | Type_mismatch -> "type mismatch"
  | Unbound_value name -> Format.sprintf "unbound value: %s" name
;;

let pp_error ppf err = Format.fprintf ppf "%s" (show_error err)

module Substitution (M : Monads.STATE_MONAD) = struct
  open Base
  open M

  type t = (string, ty, String.comparator_witness) Base.Map.t

  let empty = Map.empty (module String)
  let walk = Map.find
  let set sub name ty = Map.set sub ~key:name ~data:ty
  let remove sub name = Map.remove sub name

  let occurs_in name = function
    | TVar _ -> false
    | ty ->
      let rec helper = function
        | TUnit | TInt | TBool -> false
        | TVar name2 -> String.equal name name2
        | TProd (ty1, ty2, tys) -> List.exists ~f:helper (ty1 :: ty2 :: tys)
        | TArrow (ty1, ty2) -> helper ty1 || helper ty2
      in
      helper ty
  ;;

  let rec apply sub = function
    | (TUnit | TInt | TBool) as x -> return x
    | TVar name ->
      (match walk sub name with
       | Some ty -> return ty
       | None -> return (TVar name))
    | TProd (ty1, ty2, tys) ->
      return (fun ty1 ty2 tys -> TProd (ty1, ty2, tys))
      <*> apply sub ty1
      <*> apply sub ty2
      <*> List.fold tys ~init:(return []) ~f:(fun acc ty ->
        let* tys = acc in
        let* ty' = apply sub ty in
        return (List.rev (ty' :: tys)))
    | TArrow (ty1, ty2) ->
      return (fun ty1 ty2 -> TArrow (ty1, ty2)) <*> apply sub ty1 <*> apply sub ty2
  ;;

  let rec unify ty1 ty2 =
    match ty1, ty2 with
    | TUnit, TUnit | TInt, TInt | TBool, TBool -> return empty
    | TVar lname, TVar rname when String.equal lname rname -> return empty
    | TVar name, ty when occurs_in name ty -> fail (Occurs_check (name, ty))
    | ty, TVar name when occurs_in name ty -> fail (Occurs_check (name, ty))
    | TVar name, ty | ty, TVar name -> return (set empty name ty)
    | TProd (x1, x2, xs), TProd (y1, y2, ys) when List.length xs = List.length ys ->
      List.fold2_exn
        (x1 :: x2 :: xs)
        (y1 :: y2 :: ys)
        ~init:(return empty)
        ~f:(fun acc ty1 ty2 ->
          let* sub1 = acc in
          let* ty1' = apply sub1 ty1 in
          let* ty2' = apply sub1 ty2 in
          let* sub2 = unify ty1' ty2' in
          compose sub1 sub2)
    | TArrow (a1, b1), TArrow (a2, b2) ->
      let* sub1 = unify a1 a2 in
      let* sub2 = unify b1 b2 in
      compose sub1 sub2
    | _ -> fail (Unification_failed (ty1, ty2))

  and compose sub1 sub2 =
    Map.fold sub2 ~init:(return sub1) ~f:(fun ~key ~data acc ->
      let* sub = acc in
      extend sub key data)

  and compose_list subs =
    List.fold
      subs
      ~f:(fun acc sub1 ->
        let* sub2 = acc in
        compose sub1 sub2)
      ~init:(return empty)

  and extend sub name ty =
    if occurs_in name ty
    then fail (Occurs_check (name, ty))
    else (
      match walk sub name with
      | Some ty2 -> unify ty ty2 >>= compose sub
      | None ->
        let* sub' =
          let* ty' = apply sub ty in
          return (set empty name ty')
        in
        Map.fold sub ~init:(return sub') ~f:(fun ~key ~data acc ->
          let* acc = acc in
          let* data' = apply sub' data in
          return (Map.update acc key ~f:(fun _ -> data'))))
  ;;
end

module VarSet = struct
  include Set.Make (String)
end

module Scheme (M : Monads.STATE_MONAD) = struct
  open M
  module Substitution = Substitution (M)

  type t = Scheme of VarSet.t * ty

  let apply sub (Scheme (vars, ty)) =
    let* sub =
      VarSet.fold
        (fun name acc ->
           let* sub = acc in
           let sub' = Substitution.remove sub name in
           return sub')
        vars
        (return sub)
    in
    let* sub = Substitution.apply sub ty in
    return (Scheme (vars, sub))
  ;;
end

module Environment (M : Monads.STATE_MONAD) = struct
  open M
  open Base
  module Scheme = Scheme (M)
  module Substitution = Substitution (M)

  type t = (string, Scheme.t, String.comparator_witness) Map.t

  let empty = Map.empty (module String)
  let extend env name ty = Map.set env ~key:name ~data:ty
  let find env name = Map.find env name

  let apply sub env =
    let helper ~key:name ~data:scheme acc =
      let* env = acc in
      let* scheme' = Scheme.apply sub scheme in
      return (Map.set env ~key:name ~data:scheme')
    in
    Map.fold env ~f:helper ~init:(return env)
  ;;
end

module Infer (M : Monads.STATE_MONAD) = struct
  module Substitution = Substitution (M)
  module Environment = Environment (M)
  module Scheme = Scheme (M)
  open Scheme
  open Base
  open M

  let type_of_binop = function
    | Add | Sub | Mul | Div -> return (TInt, TInt, TInt)
    | Eq | Ne | Le | Ge | Lt | Gt -> return (TBool, TInt, TInt)
    | _ -> fail (Not_implemented "type of binop")
  ;;

  let fresh_tvar = fresh_str >>= fun name -> return (TVar name)

  let rec pattern env = function
    | PAny ->
      let* ty = fresh_tvar in
      return (env, ty)
    | PVar name ->
      let* ty = fresh_tvar in
      let scheme = Scheme (VarSet.empty, ty) in
      let env' = Environment.extend env name scheme in
      return (env', ty)
    | PTuple (p1, p2, ps) ->
      let helper acc patt =
        let* env, tys = acc in
        let* env, ty = pattern env patt in
        return (env, ty :: tys)
      in
      let* env, ty1 = pattern env p1 in
      let* env, ty2 = pattern env p2 in
      let* env, tys = List.fold ps ~init:(return (env, [])) ~f:helper in
      return (env, TProd (ty1, ty2, tys))
    | _ -> fail (Not_implemented "pattern")
  ;;

  let rec expression env = function
    | EConstant CUnit -> return (Substitution.empty, TUnit)
    | EConstant (CBool _) -> return (Substitution.empty, TBool)
    | EConstant (CInt _) -> return (Substitution.empty, TInt)
    | EVar name ->
      (match Environment.find env name with
       | None -> fail (Unbound_value name)
       | Some (Scheme (var_set, ty)) ->
         let helper =
           fun name acc ->
           let* ty = acc in
           let* gen_var = fresh_tvar in
           Substitution.apply (Substitution.set Substitution.empty name gen_var) ty
         in
         let* ty = VarSet.fold helper var_set (return ty) in
         return (Substitution.empty, ty))
    | EBinop (op, e1, e2) ->
      let* sub1, ty1 = expression env e1 in
      let* env1 = Environment.apply sub1 env in
      let* sub2, ty2 = expression env1 e2 in
      let* ty, left_ty, right_ty = type_of_binop op in
      let* ty1' = Substitution.apply sub1 ty1 in
      let* ty2' = Substitution.apply sub2 ty2 in
      let* sub1' = Substitution.unify ty1' left_ty in
      let* sub2' = Substitution.unify ty2' right_ty in
      let* sub = Substitution.compose sub1' sub2' in
      return (sub, ty)
    | ETuple (e1, e2, es) ->
      let* sub1, ty1 = expression env e1 in
      let* env = Environment.apply sub1 env in
      let* sub2, ty2 = expression env e2 in
      let* sub, tys =
        List.fold
          es
          ~init:(return (sub2, []))
          ~f:(fun acc e ->
            let* sub, tys = acc in
            let* env = Environment.apply sub env in
            let* sub', ty = expression env e in
            return (sub', ty :: tys))
      in
      let ty = TProd (ty1, ty2, List.rev tys) in
      return (sub, ty)
    | EApp (e1, e2) ->
      let* sub1, ty1 = expression env e1 in
      let* sub2, ty2 = expression env e2 in
      let* sub12 = Substitution.compose sub1 sub2 in
      let* ty1' = Substitution.apply sub12 ty1 in
      let* ty2' = Substitution.apply sub12 ty2 in
      let* tv = fresh_tvar in
      let* sub3 = Substitution.unify ty1' (TArrow (ty2', tv)) in
      let* sub = Substitution.compose sub12 sub3 in
      let* ty = Substitution.apply sub tv in
      return (sub, ty)
    | EFun (p, e) ->
      let* env, ty1 = pattern env p in
      let* sub, ty2 = expression env e in
      let ty12 = TArrow (ty1, ty2) in
      let* ty = Substitution.apply sub ty12 in
      return (sub, ty)
    | EIf (cond, expr_then, expr_else) ->
      let* sub1, ty1 = expression env cond in
      let* sub2 = Substitution.unify ty1 TBool in
      let* sub3, ty2 = expression env expr_then in
      let* sub4, ty3 = expression env expr_else in
      let* sub5 = Substitution.unify ty2 ty3 in
      let* sub = Substitution.compose_list [ sub1; sub2; sub3; sub4; sub5 ] in
      let* ty = Substitution.apply sub ty2 in
      return (sub, ty)
    | ELet (NonRecursive, (vb, vbs), body) ->
      let helper env (patt, expr) =
        let* env = env in
        let* env, ty1 = pattern env patt in
        let* sub1, ty2 = expression env expr in
        let* sub2 = Substitution.unify ty1 ty2 in
        let* sub = Substitution.compose sub1 sub2 in
        Environment.apply sub env
      in
      let* env = List.fold (vb :: vbs) ~f:helper ~init:(return env) in
      expression env body
    | _ -> fail (Not_implemented "expression")
  ;;

  let expression ast =
    match M.run (expression Environment.empty ast) with
    | Ok (_state, (_sub, ty)) -> Ok ty
    | Error e -> Error e
  ;;
end
