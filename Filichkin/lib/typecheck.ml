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
  | TVar of tvar ref

and tvar =
  | Unbound of int
  | Link of typ

type scheme = Forall of int list * typ

type type_env =
  { vars : (string * scheme) list
  ; ctors : (string * scheme) list
  ; types : (string * string list) list
  ; type_def_ctors : (string * string list) list
  }

exception TypeError of string

let fresh_tyvar =
  let counter = ref 0 in
  fun () ->
    incr counter;
    TVar (ref (Unbound !counter))
;;

let lookup_var x env = List.assoc_opt x env.vars
let lookup_ctor c env = List.assoc_opt c env.ctors
let extend_vars env xs = { env with vars = xs @ env.vars }
let extend_ctors env cs = { env with ctors = cs @ env.ctors }

let rec prune = function
  | TVar ({ contents = Link t } as r) ->
    let t' = prune t in
    r := Link t';
    t'
  | t -> t
;;

let rec occurs id = function
  | TInt | TBool | TUnit -> false
  | TFun (t1, t2) -> occurs id t1 || occurs id t2
  | TTuple ts -> List.exists (occurs id) ts
  | TCon (_, ts) -> List.exists (occurs id) ts
  | TVar { contents = Link t } -> occurs id t
  | TVar { contents = Unbound v_id } -> id = v_id
;;

let rec unify t1 t2 =
  let t1, t2 = prune t1, prune t2 in
  match t1, t2 with
  | TInt, TInt | TBool, TBool | TUnit, TUnit -> ()
  | TFun (a1, r1), TFun (a2, r2) ->
    unify a1 a2;
    unify r1 r2
  | TTuple ts1, TTuple ts2 ->
    if List.length ts1 <> List.length ts2 then raise (TypeError "tuple length mismatch");
    List.iter2 unify ts1 ts2
  | TCon (n1, ts1), TCon (n2, ts2) when n1 = n2 ->
    if List.length ts1 <> List.length ts2
    then raise (TypeError "type constructor arity mismatch");
    List.iter2 unify ts1 ts2
  | TVar ({ contents = Unbound id } as r), t | t, TVar ({ contents = Unbound id } as r) ->
    if occurs id t then raise (TypeError "recursive type") else r := Link t
  | _ -> raise (TypeError "type mismatch")
;;

module IntSet = Set.Make (Int)

let rec free_tyvars = function
  | TInt | TBool | TUnit -> IntSet.empty
  | TFun (t1, t2) -> IntSet.union (free_tyvars t1) (free_tyvars t2)
  | TTuple ts | TCon (_, ts) ->
    List.fold_left (fun acc t -> IntSet.union acc (free_tyvars t)) IntSet.empty ts
  | TVar { contents = Link t } -> free_tyvars t
  | TVar { contents = Unbound id } -> IntSet.singleton id
;;

let generalize env t =
  let env_fv =
    List.fold_left
      (fun acc (_, Forall (_, t)) -> IntSet.union acc (free_tyvars t))
      IntSet.empty
      env
  in
  let t_fv = free_tyvars t in
  let vars = IntSet.elements (IntSet.diff t_fv env_fv) in
  Forall (vars, t)
;;

let instantiate (Forall (vars, t)) =
  let subst = List.map (fun id -> id, fresh_tyvar ()) vars in
  let rec go = function
    | (TInt | TBool | TUnit) as t -> t
    | TFun (t1, t2) -> TFun (go t1, go t2)
    | TTuple ts -> TTuple (List.map go ts)
    | TCon (name, ts) -> TCon (name, List.map go ts)
    | TVar { contents = Link t } -> go t
    | TVar { contents = Unbound id } ->
      (match List.assoc_opt id subst with
       | Some t -> t
       | None -> fresh_tyvar ())
  in
  go t
;;

let rec bind_pattern env p t =
  match p, prune t with
  | PVar x, t -> [ x, t ]
  | PTuple ps, TTuple ts ->
    if List.length ps <> List.length ts
    then raise (TypeError "tuple pattern length mismatch");
    List.concat (List.map2 (bind_pattern env) ps ts)
  | PTuple ps, TVar ({ contents = Unbound _ } as r) ->
    let element_tys = List.map (fun _ -> fresh_tyvar ()) ps in
    r := Link (TTuple element_tys);
    List.concat (List.map2 (bind_pattern env) ps element_tys)
  | PConstr (name, ps), t ->
    (match lookup_ctor name env with
     | None -> raise (TypeError ("unknown constructor " ^ name))
     | Some scheme ->
       let ctor_ty = instantiate scheme in
       let rec match_args pats ty =
         match pats, prune ty with
         | [], res_ty ->
           unify t res_ty;
           []
         | p :: ps, TFun (arg_ty, rest_ty) ->
           let bs = bind_pattern env p arg_ty in
           bs @ match_args ps rest_ty
         | _ -> raise (TypeError "constructor arity mismatch")
       in
       match_args ps ctor_ty)
  | _ -> raise (TypeError "pattern matching error: type mismatch")
;;

type coverage =
  | All
  | Ctors of string list
  | Other

let check_exhaustive_match env scrutinee_ty cases =
  match prune scrutinee_ty with
  | TCon (type_name, _) ->
    (match List.assoc_opt type_name env.type_def_ctors with
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
              (TypeError ("non-exhaustive match, missing: " ^ String.concat ", " missing))
        | Other -> ()))
  | _ -> ()
;;

let rec infer env = function
  | Int _ -> TInt
  | Bool _ -> TBool
  | Var x ->
    (match lookup_var x env with
     | Some scheme -> instantiate scheme
     | None -> raise (TypeError ("unbound variable " ^ x)))
  | Constr c ->
    (match lookup_ctor c env with
     | Some scheme -> instantiate scheme
     | None -> raise (TypeError ("unknown constructor " ^ c)))
  | Abs (pat, body) ->
    let tv = fresh_tyvar () in
    let bindings = bind_pattern env pat tv in
    let env' = extend_vars env (List.map (fun (x, t) -> x, Forall ([], t)) bindings) in
    TFun (tv, infer env' body)
  | App (f, arg) ->
    let tf, ta = infer env f, infer env arg in
    let tr = fresh_tyvar () in
    unify tf (TFun (ta, tr));
    tr
  | Tuple es -> TTuple (List.map (infer env) es)
  | Let (NonRec, pat, e1, body_opt) ->
    let t1 = infer env e1 in
    let bindings = bind_pattern env pat t1 in
    let new_vars = List.map (fun (x, t) -> x, generalize env.vars t) bindings in
    let env' = extend_vars env new_vars in
    (match body_opt with
     | Some e2 -> infer env' e2
     | None -> TUnit)
  | Let (Rec, PVar x, e1, body_opt) ->
    let tv = fresh_tyvar () in
    let env_pre = extend_vars env [ x, Forall ([], tv) ] in
    let t1 = infer env_pre e1 in
    unify tv t1;
    let env' = extend_vars env [ x, generalize env_pre.vars t1 ] in
    (match body_opt with
     | Some e2 -> infer env' e2
     | None -> TUnit)
  | Let (Rec, _, _, _) -> raise (TypeError "recursive let only supports simple variables")
  | If (c, t, e) ->
    unify (infer env c) TBool;
    let tt = infer env t in
    unify tt (infer env e);
    tt
  | BinOp (op, e1, e2) ->
    let t1, t2 = infer env e1, infer env e2 in
    let req1, req2, res =
      match op with
      | Plus | Minus | Mult | Div -> TInt, TInt, TInt
      | Equal | NotEqual | More | Less | EMore | ELess -> TInt, TInt, TBool
      | And | Or -> TBool, TBool, TBool
    in
    unify t1 req1;
    unify t2 req2;
    res
  | UnOp (op, e) ->
    let t = infer env e in
    let req, res =
      match op with
      | Neg -> TInt, TInt
      | Not -> TBool, TBool
    in
    unify t req;
    res
  | Match (scrutinee, cases) ->
    let t_scrut = infer env scrutinee in
    check_exhaustive_match env t_scrut cases;
    let t_res = fresh_tyvar () in
    List.iter
      (fun (pat, expr) ->
        let t_pat = fresh_tyvar () in
        unify t_pat t_scrut;
        let bindings = bind_pattern env pat t_pat in
        let env' =
          extend_vars env (List.map (fun (x, t) -> x, Forall ([], t)) bindings)
        in
        unify (infer env' expr) t_res)
      cases;
    t_res
;;

let rec ast_to_typ tp_env = function
  | TEInt -> TInt
  | TEBool -> TBool
  | TEUnit -> TUnit
  | TEVar name ->
    (match List.assoc_opt name tp_env with
     | Some t -> t
     | None -> raise (TypeError ("unbound type parameter " ^ name)))
  | TEArrow (t1, t2) -> TFun (ast_to_typ tp_env t1, ast_to_typ tp_env t2)
  | TETuple ts -> TTuple (List.map (ast_to_typ tp_env) ts)
  | TEConstr (name, args) -> TCon (name, List.map (ast_to_typ tp_env) args)
;;

type tc_state =
  { tenv : type_env
  ; last_type : typ option
  }

let process_type_decl env td =
  let distinct_vars = List.map (fun name -> name, fresh_tyvar ()) td.type_params in
  let forall_vars =
    List.map
      (function
        | _, TVar { contents = Unbound id } -> id
        | _ -> failwith "impossible")
      distinct_vars
  in
  let result_type = TCon (td.type_name, List.map snd distinct_vars) in
  let new_ctors =
    List.map
      (fun c ->
        let arg_types = List.map (ast_to_typ distinct_vars) c.ctor_args in
        let ctor_ty =
          List.fold_right (fun arg acc -> TFun (arg, acc)) arg_types result_type
        in
        c.ctor_name, Forall (forall_vars, ctor_ty))
      td.constructors
  in
  let env = extend_ctors env new_ctors in
  { env with
    types = (td.type_name, td.type_params) :: env.types
  ; type_def_ctors =
      (td.type_name, List.map (fun c -> c.ctor_name) td.constructors)
      :: env.type_def_ctors
  }
;;

let check_toplevel state tl =
  try
    match tl with
    | TLType td ->
      Ok { state with tenv = process_type_decl state.tenv td; last_type = None }
    | TLExpr (Let (NonRec, pat, expr, None)) ->
      let ty = infer state.tenv expr in
      let bindings = bind_pattern state.tenv pat ty in
      let new_vars = List.map (fun (n, t) -> n, generalize state.tenv.vars t) bindings in
      Ok { tenv = extend_vars state.tenv new_vars; last_type = Some ty }
    | TLExpr (Let (Rec, PVar name, expr, None)) ->
      let tv = fresh_tyvar () in
      let env_pre = extend_vars state.tenv [ name, Forall ([], tv) ] in
      let t_expr = infer env_pre expr in
      unify tv t_expr;
      let scheme = generalize state.tenv.vars t_expr in
      Ok { tenv = extend_vars state.tenv [ name, scheme ]; last_type = Some t_expr }
    | TLExpr (Let (Rec, _, _, None)) ->
      Error "Recursive let only supports simple variables"
    | TLExpr e -> Ok { state with last_type = Some (infer state.tenv e) }
  with
  | TypeError msg -> Error msg
;;

let initial_env =
  let a = fresh_tyvar () in
  let option_a = TCon ("option", [ a ]) in
  let option_ctors =
    [ "None", Forall ([], option_a); "Some", Forall ([], TFun (a, option_a)) ]
  in
  { vars =
      [ "print_int", Forall ([], TFun (TInt, TUnit))
      ; "print_bool", Forall ([], TFun (TBool, TUnit))
      ; "true", Forall ([], TBool)
      ; "false", Forall ([], TBool)
      ]
  ; ctors = option_ctors
  ; types = [ "option", [ "a" ] ]
  ; type_def_ctors = [ "option", [ "None"; "Some" ] ]
  }
;;

let initial_tc_state = { tenv = initial_env; last_type = None }

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
  | TVar { contents = Link t } -> string_of_type t
  | TVar { contents = Unbound id } ->
    let rec id_to_str n =
      let letter = String.make 1 (Char.chr (97 + (n mod 26))) in
      if n < 26 then "'" ^ letter else "{" ^ id_to_str ((n / 26) - 1) ^ letter
    in
    id_to_str id
;;
