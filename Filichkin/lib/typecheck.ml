[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast

type typ =
  | TInt
  | TBool
  | TUnit
  | TFun of typ * typ
  | TTuple of typ list
  | TCon of string * typ list
  | TVar of int

type scheme = Forall of int list * typ

module StringMap = Map.Make (String)

type type_env =
  { vars : scheme StringMap.t
  ; ctors : scheme StringMap.t
  ; types : string list StringMap.t
  ; type_def_ctors : string list StringMap.t
  }

exception TypeError of string

module IntMap = Map.Make (Int)
module IntSet = Set.Make (Int)

type subst = typ IntMap.t

type infer_state =
  { next_var : int
  ; subst : subst
  }

let empty_subst = IntMap.empty
let map_of_list xs =
  List.fold_left (fun acc (k, v) -> StringMap.add k v acc) StringMap.empty xs
;;

let add_bindings map xs =
  List.fold_left (fun acc (k, v) -> StringMap.add k v acc) map xs
;;

let lookup_var x env = StringMap.find_opt x env.vars
let lookup_ctor c env = StringMap.find_opt c env.ctors
let lookup_type name env = StringMap.find_opt name env.types
let extend_vars env xs = { env with vars = add_bindings env.vars xs }
let extend_ctors env cs = { env with ctors = add_bindings env.ctors cs }

let rec occurs id = function
  | TInt | TBool | TUnit -> false
  | TFun (t1, t2) -> occurs id t1 || occurs id t2
  | TTuple ts -> List.exists (occurs id) ts
  | TCon (_, ts) -> List.exists (occurs id) ts
  | TVar v -> v = id
;;

let apply_subst subst ty =
  let rec go seen = function
    | TInt | TBool | TUnit as t -> t
    | TFun (t1, t2) -> TFun (go seen t1, go seen t2)
    | TTuple ts -> TTuple (List.map (go seen) ts)
    | TCon (name, ts) -> TCon (name, List.map (go seen) ts)
    | TVar id ->
      if IntSet.mem id seen
      then TVar id
      else (
        match IntMap.find_opt id subst with
        | None -> TVar id
        | Some t -> if occurs id t then TVar id else go (IntSet.add id seen) t)
  in
  go IntSet.empty ty
;;

let apply_subst_scheme subst (Forall (vars, t)) =
  let subst = List.fold_left (fun acc v -> IntMap.remove v acc) subst vars in
  Forall (vars, apply_subst subst t)
;;

let apply_subst_env subst env =
  { env with
    vars = StringMap.map (apply_subst_scheme subst) env.vars
  ; ctors = StringMap.map (apply_subst_scheme subst) env.ctors
  }
;;

let extend_subst id t subst =
  let t = apply_subst subst t in
  if t = TVar id
  then subst
  else (
    let subst = IntMap.map (fun ty -> apply_subst (IntMap.singleton id t) ty) subst in
    IntMap.add id t subst)
;;

let rec unify subst t1 t2 =
  let t1 = apply_subst subst t1 in
  let t2 = apply_subst subst t2 in
  match t1, t2 with
  | TInt, TInt | TBool, TBool | TUnit, TUnit -> subst
  | TFun (a1, r1), TFun (a2, r2) ->
    let subst = unify subst a1 a2 in
    unify subst r1 r2
  | TTuple ts1, TTuple ts2 ->
    if List.length ts1 <> List.length ts2 then raise (TypeError "tuple length mismatch");
    List.fold_left2 unify subst ts1 ts2
  | TCon (n1, ts1), TCon (n2, ts2) when n1 = n2 ->
    if List.length ts1 <> List.length ts2
    then raise (TypeError "type constructor arity mismatch");
    List.fold_left2 unify subst ts1 ts2
  | TVar id, t | t, TVar id -> extend_subst id t subst
  | _ -> raise (TypeError "type mismatch")
;;

let rec free_tyvars = function
  | TInt | TBool | TUnit -> IntSet.empty
  | TFun (t1, t2) -> IntSet.union (free_tyvars t1) (free_tyvars t2)
  | TTuple ts | TCon (_, ts) ->
    List.fold_left (fun acc t -> IntSet.union acc (free_tyvars t)) IntSet.empty ts
  | TVar id -> IntSet.singleton id
;;

let free_scheme (Forall (vars, t)) = IntSet.diff (free_tyvars t) (IntSet.of_list vars)

let generalize env_vars t =
  let env_fv =
    StringMap.fold
      (fun _ sch acc -> IntSet.union acc (free_scheme sch))
      env_vars
      IntSet.empty
  in
  let t_fv = free_tyvars t in
  let vars = IntSet.elements (IntSet.diff t_fv env_fv) in
  Forall (vars, t)
;;

let fresh_tyvar st = TVar st.next_var, { st with next_var = st.next_var + 1 }

let instantiate st (Forall (vars, t)) =
  let rec build st subst = function
    | [] -> subst, st
    | id :: rest ->
      let tv, st = fresh_tyvar st in
      build st (IntMap.add id tv subst) rest
  in
  let subst, st = build st IntMap.empty vars in
  apply_subst subst t, st
;;

let unify_state st t1 t2 = { st with subst = unify st.subst t1 t2 }

let rec bind_pattern st env pat ty =
  let ty = apply_subst st.subst ty in
  match pat, ty with
  | PVar x, t -> st, [ x, t ]
  | PWildcard, _ -> st, []
  | PTuple ps, TTuple ts ->
    if List.length ps <> List.length ts
    then raise (TypeError "tuple pattern length mismatch");
    List.fold_left2
      (fun (st, acc) p t ->
        let st, bs = bind_pattern st env p t in
        st, acc @ bs)
      (st, [])
      ps
      ts
  | PTuple ps, TVar _ ->
    let rec fresh_list st acc = function
      | 0 -> List.rev acc, st
      | n ->
        let tv, st = fresh_tyvar st in
        fresh_list st (tv :: acc) (n - 1)
    in
    let elem_tys, st = fresh_list st [] (List.length ps) in
    let st = unify_state st ty (TTuple elem_tys) in
    List.fold_left2
      (fun (st, acc) p t ->
        let st, bs = bind_pattern st env p t in
        st, acc @ bs)
      (st, [])
      ps
      elem_tys
  | PConstr (name, ps), t ->
    (match lookup_ctor name env with
     | None -> raise (TypeError ("unknown constructor " ^ name))
     | Some scheme ->
       let scheme = apply_subst_scheme st.subst scheme in
       let ctor_ty, st = instantiate st scheme in
       let rec match_args st pats ty =
         let ty = apply_subst st.subst ty in
         match pats, ty with
         | [], res_ty ->
           let st = unify_state st t res_ty in
           st, []
         | p :: ps, TFun (arg_ty, rest_ty) ->
           let st, bs1 = bind_pattern st env p arg_ty in
           let st, bs2 = match_args st ps rest_ty in
           st, bs1 @ bs2
         | _ -> raise (TypeError "constructor arity mismatch")
       in
       match_args st ps ctor_ty)
  | _ -> raise (TypeError "pattern matching error: type mismatch")
;;

type coverage =
  | All
  | Ctors of string list

let check_exhaustive_match env scrutinee_ty cases =
  match scrutinee_ty with
  | TCon (type_name, _) ->
    (match StringMap.find_opt type_name env.type_def_ctors with
     | None -> ()
     | Some all_ctors ->
       let covered =
         List.fold_left
           (fun acc (pat, _) ->
             match acc, pat with
             | All, _ -> All
             | _, PWildcard | _, PVar _ -> All
             | Ctors cs, PConstr (name, _) ->
               if List.mem name cs then Ctors cs else Ctors (name :: cs)
             | _ -> acc)
           (Ctors [])
           cases
       in
       (match covered with
        | All -> ()
        | Ctors cs ->
          let missing = List.filter (fun c -> not (List.mem c cs)) all_ctors in
          if missing <> []
          then
            raise
              (TypeError ("non-exhaustive match, missing: " ^ String.concat ", " missing))))
  | _ -> ()
;;

let rec infer_expr st env = function
  | Int _ -> TInt, st
  | Bool _ -> TBool, st
  | Var x ->
    (match lookup_var x env with
     | Some scheme ->
       let scheme = apply_subst_scheme st.subst scheme in
       instantiate st scheme
     | None -> raise (TypeError ("unbound variable " ^ x)))
  | Constr c ->
    (match lookup_ctor c env with
     | Some scheme ->
       let scheme = apply_subst_scheme st.subst scheme in
       instantiate st scheme
     | None -> raise (TypeError ("unknown constructor " ^ c)))
  | Abs (pat, body) ->
    let tv, st = fresh_tyvar st in
    let st, bindings = bind_pattern st env pat tv in
    let env' = extend_vars env (List.map (fun (x, t) -> x, Forall ([], t)) bindings) in
    let t_body, st = infer_expr st env' body in
    TFun (apply_subst st.subst tv, t_body), st
  | App (f, arg) ->
    let tf, st = infer_expr st env f in
    let ta, st = infer_expr st env arg in
    let tr, st = fresh_tyvar st in
    let st = unify_state st tf (TFun (ta, tr)) in
    apply_subst st.subst tr, st
  | Tuple es ->
    let rec infer_list st acc = function
      | [] -> List.rev acc, st
      | e :: es ->
        let t, st = infer_expr st env e in
        infer_list st (t :: acc) es
    in
    let ts, st = infer_list st [] es in
    TTuple ts, st
  | Let (NonRec, pat, e1, body_opt) ->
    let t1, st = infer_expr st env e1 in
    let t1 = apply_subst st.subst t1 in
    let env = apply_subst_env st.subst env in
    let st, bindings = bind_pattern st env pat t1 in
    let env = apply_subst_env st.subst env in
    let new_vars =
      List.map
        (fun (x, t) -> x, generalize env.vars (apply_subst st.subst t))
        bindings
    in
    let env' = extend_vars env new_vars in
    (match body_opt with
     | Some e2 -> infer_expr st env' e2
     | None -> TUnit, st)
  | Let (Rec, pat, e1, body_opt) ->
    (match pat, e1 with
     | PVar name, Abs _ ->
       let tv, st = fresh_tyvar st in
       let env_pre = extend_vars env [ name, Forall ([], tv) ] in
       let t1, st = infer_expr st env_pre e1 in
       let st = unify_state st tv t1 in
       let env = apply_subst_env st.subst env in
       let t1 = apply_subst st.subst t1 in
       let scheme = generalize env.vars t1 in
       let env' = extend_vars env [ name, scheme ] in
       (match body_opt with
        | Some e2 -> infer_expr st env' e2
        | None -> TUnit, st)
     | PVar _, _ ->
       (match body_opt with
        | Some _ -> raise (TypeError "recursive let-in must bind function")
        | None -> raise (TypeError "recursive binding must be a function"))
     | _ -> raise (TypeError "Recursive let-tuple is not supported"))
  | If (c, t, e) ->
    let tc, st = infer_expr st env c in
    let st = unify_state st tc TBool in
    let tt, st = infer_expr st env t in
    let te, st = infer_expr st env e in
    let st = unify_state st tt te in
    apply_subst st.subst tt, st
  | BinOp (op, e1, e2) ->
    let t1, st = infer_expr st env e1 in
    let t2, st = infer_expr st env e2 in
    let req1, req2, res =
      match op with
      | Plus | Minus | Mult | Div -> TInt, TInt, TInt
      | Equal | NotEqual | More | Less | EMore | ELess -> TInt, TInt, TBool
      | And | Or -> TBool, TBool, TBool
    in
    let st = unify_state st t1 req1 in
    let st = unify_state st t2 req2 in
    res, st
  | UnOp (op, e) ->
    let t, st = infer_expr st env e in
    let req, res =
      match op with
      | Neg -> TInt, TInt
      | Not -> TBool, TBool
    in
    let st = unify_state st t req in
    res, st
  | Match (scrutinee, cases) ->
    let t_scrut, st = infer_expr st env scrutinee in
    let t_res, st = fresh_tyvar st in
    let st =
      List.fold_left
        (fun st (pat, expr) ->
          let t_pat, st = fresh_tyvar st in
          let st = unify_state st t_pat t_scrut in
          let st, bindings = bind_pattern st env pat t_pat in
          let env' =
            extend_vars env (List.map (fun (x, t) -> x, Forall ([], t)) bindings)
          in
          let t_expr, st = infer_expr st env' expr in
          unify_state st t_expr t_res)
        st
        cases
    in
    let t_scrut = apply_subst st.subst t_scrut in
    check_exhaustive_match env t_scrut cases;
    apply_subst st.subst t_res, st
;;

let rec ast_to_typ env tp_env = function
  | TEInt -> TInt
  | TEBool -> TBool
  | TEUnit -> TUnit
  | TEVar name ->
    (match List.assoc_opt name tp_env with
     | Some t -> t
     | None -> raise (TypeError ("unbound type parameter " ^ name)))
  | TEArrow (t1, t2) -> TFun (ast_to_typ env tp_env t1, ast_to_typ env tp_env t2)
  | TETuple ts -> TTuple (List.map (ast_to_typ env tp_env) ts)
  | TEConstr (name, args) ->
    (match lookup_type name env with
     | None -> raise (TypeError ("unknown type " ^ name))
     | Some params ->
       if List.length args <> List.length params
       then raise (TypeError "type constructor arity mismatch")
       else TCon (name, List.map (ast_to_typ env tp_env) args))
;;

type tc_state =
  { tenv : type_env
  ; last_type : typ option
  ; next_var : int
  }

let process_type_decl env next_var td =
  let env_with_type =
    { env with types = StringMap.add td.type_name td.type_params env.types }
  in
  let rec fresh_params next acc = function
    | [] -> List.rev acc, next
    | name :: rest ->
      let t = TVar next in
      fresh_params (next + 1) ((name, t) :: acc) rest
  in
  let distinct_vars, next_var = fresh_params next_var [] td.type_params in
  let forall_vars =
    List.map
      (function
        | _, TVar id -> id
        | _ -> failwith "impossible")
      distinct_vars
  in
  let result_type = TCon (td.type_name, List.map snd distinct_vars) in
  let new_ctors =
    List.map
      (fun c ->
        let arg_types = List.map (ast_to_typ env_with_type distinct_vars) c.ctor_args in
        let ctor_ty =
          List.fold_right (fun arg acc -> TFun (arg, acc)) arg_types result_type
        in
        c.ctor_name, Forall (forall_vars, ctor_ty))
      td.constructors
  in
  let env_with_ctors = extend_ctors env_with_type new_ctors in
  ( { env_with_ctors with
      type_def_ctors =
        StringMap.add
          td.type_name
          (List.map (fun c -> c.ctor_name) td.constructors)
          env.type_def_ctors
    }
  , next_var )
;;

let check_toplevel state tl =
  try
    match tl with
    | TLType td ->
      let tenv, next_var = process_type_decl state.tenv state.next_var td in
      Ok { tenv; last_type = None; next_var }
    | TLExpr (Let (NonRec, pat, expr, None)) ->
      let st = { next_var = state.next_var; subst = empty_subst } in
      let ty, st = infer_expr st state.tenv expr in
      let ty = apply_subst st.subst ty in
      let env = apply_subst_env st.subst state.tenv in
      let st, bindings = bind_pattern st env pat ty in
      let env = apply_subst_env st.subst env in
      let ty = apply_subst st.subst ty in
      let new_vars =
        List.map
          (fun (n, t) -> n, generalize env.vars (apply_subst st.subst t))
          bindings
      in
      let scheme = generalize env.vars ty in
      let ty, st = instantiate st scheme in
      Ok { tenv = extend_vars env new_vars; last_type = Some ty; next_var = st.next_var }
    | TLExpr (Let (Rec, PVar name, expr, None)) ->
      (match expr with
       | Abs _ ->
         let st = { next_var = state.next_var; subst = empty_subst } in
         let tv, st = fresh_tyvar st in
         let env_pre = extend_vars state.tenv [ name, Forall ([], tv) ] in
         let t_expr, st = infer_expr st env_pre expr in
         let st = unify_state st tv t_expr in
         let env = apply_subst_env st.subst state.tenv in
         let t_expr = apply_subst st.subst t_expr in
         let scheme = generalize env.vars t_expr in
         let env' = extend_vars env [ name, scheme ] in
         let ty, st = instantiate st scheme in
         Ok { tenv = env'; last_type = Some ty; next_var = st.next_var }
       | _ -> Error "recursive binding must be a function")
    | TLExpr (Let (Rec, _, _, None)) ->
      Error "Recursive let-tuple is not supported"
    | TLExpr e ->
      let st = { next_var = state.next_var; subst = empty_subst } in
      let ty, st = infer_expr st state.tenv e in
      let ty = apply_subst st.subst ty in
      Ok { state with last_type = Some ty; next_var = st.next_var }
  with
  | TypeError msg -> Error msg
;;

let initial_env, initial_next_var =
  let option_var = 0 in
  let next_var = option_var + 1 in
  let option_a = TCon ("option", [ TVar option_var ]) in
  let option_ctors =
    [ "None", Forall ([ option_var ], option_a)
    ; "Some", Forall ([ option_var ], TFun (TVar option_var, option_a))
    ]
  in
    ( { vars =
        map_of_list
          [ "print_int", Forall ([], TFun (TInt, TUnit))
          ; "println_int", Forall ([], TFun (TInt, TUnit))
          ; "print_bool", Forall ([], TFun (TBool, TUnit))
          ; "true", Forall ([], TBool)
          ; "false", Forall ([], TBool)
          ]
    ; ctors = map_of_list option_ctors
    ; types = map_of_list [ "option", [ "a" ] ]
    ; type_def_ctors = map_of_list [ "option", [ "None"; "Some" ] ]
    }
  , next_var )
;;

let initial_state = { tenv = initial_env; last_type = None; next_var = initial_next_var }

let typecheck_toplevel state tl = check_toplevel state tl

let typecheck_program state toplevels =
  let rec loop state = function
    | [] -> Ok state
    | tl :: tls ->
      (match typecheck_toplevel state tl with
       | Error err -> Error err
       | Ok state' -> loop state' tls)
  in
  loop state toplevels
;;

let get_last_type state = state.last_type

let infer env expr =
  let st = { next_var = 0; subst = empty_subst } in
  let ty, st = infer_expr st env expr in
  apply_subst st.subst ty
;;

let rec string_of_type = function
  | TInt -> "int"
  | TBool -> "bool"
  | TUnit -> "unit"
  | TFun ((TFun _ as t1), t2) ->
    Printf.sprintf "(%s) -> %s" (string_of_type t1) (string_of_type t2)
  | TFun (t1, t2) -> Printf.sprintf "%s -> %s" (string_of_type t1) (string_of_type t2)
  | TTuple ts -> "(" ^ String.concat " * " (List.map string_of_type ts) ^ ")"
  | TCon (name, []) -> name
  | TCon (name, ts) ->
    Printf.sprintf "%s(%s)" name (String.concat ", " (List.map string_of_type ts))
  | TVar id ->
    let rec id_to_str n =
      let letter = String.make 1 (Char.chr (97 + (n mod 26))) in
      if n < 26 then "'" ^ letter else "{" ^ id_to_str ((n / 26) - 1) ^ letter
    in
    id_to_str id
;;
