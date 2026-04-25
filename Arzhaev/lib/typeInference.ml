[@@@ocaml.text "/*"]

(** Copyright 2026, Dmitry Arzhaev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast
open Utils
module StrSet = SetWithToList (Set.Make (String))

type ground =
  | GInt
  | GBool
  | GFloat

type typ =
  | TGround of ground
  | TVar of string
  | TArrow of typ * typ

type scheme =
  { vars : string list
  ; ty : typ
  }

type toplevel_result =
  | RLet of string * scheme
  | RExpr of typ

type subst = typ Table.t

type state =
  { sub : subst
  ; fresh : typ
  }

type infer_error =
  | IUnboundValue of string
  | IOccursCheck of string * typ
  | ITypeMismatch of typ * typ
  | ITypeError

let pp_ground fmt = function
  | GInt -> Format.fprintf fmt "int"
  | GBool -> Format.fprintf fmt "bool"
  | GFloat -> Format.fprintf fmt "float"
;;

let rec pp_typ fmt = function
  | TGround g -> pp_ground fmt g
  | TVar v -> Format.fprintf fmt "%s" v
  | TArrow (l, r) ->
    (* левая часть: скобки, если это тоже стрелка *)
    (match l with
     | TArrow _ -> Format.fprintf fmt "(%a)" pp_typ l
     | _ -> Format.fprintf fmt "%a" pp_typ l);
    Format.fprintf fmt " -> %a" pp_typ r
;;

let pp_infer_error fmt = function
  | IUnboundValue x -> Format.fprintf fmt "unbound value: %s" x
  | IOccursCheck (x, ty) -> Format.fprintf fmt "occurs check failed: %s in %a" x pp_typ ty
  | ITypeMismatch (t1, t2) ->
    Format.fprintf fmt "Type mismatch: %a vs %a" pp_typ t1 pp_typ t2
  | ITypeError -> Format.fprintf fmt "type error"
;;

type 'a infresult =
  | Failed of infer_error
  | Ok of state * 'a

type 'a inf = state -> 'a infresult

let pp_scheme fmt { vars; ty } =
  match vars with
  | [] -> Format.fprintf fmt "%a" pp_typ ty
  | _ ->
    Format.fprintf fmt "forall ";
    List.iter (fun v -> Format.fprintf fmt "%s " v) vars;
    Format.fprintf fmt ". %a" pp_typ ty
;;

let pp_infer_result pp_val fmt = function
  | Failed err -> Format.fprintf fmt "Error: %a" pp_infer_error err
  | Ok (_, v) -> Format.fprintf fmt "%a" pp_val v
;;

let pp_toplevel_result fmt = function
  | RExpr ty -> Format.fprintf fmt "- : %a" pp_typ ty
  | RLet (x, sch) -> Format.fprintf fmt "val %s : %a" x pp_scheme sch
;;

let ( >>= ) m f st =
  match m st with
  | Failed s -> Failed s
  | Ok (st', x) -> f x st'
;;

let ( let* ) = ( >>= )
let return x st = Ok (st, x)
let read st = Ok (st, st)
let write st (_ : state) = Ok (st, ())
let fail err _ = Failed err
let run f st = f st
let initial_state = { sub = Table.empty; fresh = TVar "'a" }

let get_next_tv = function
  | TVar s ->
    let rec helper = function
      | '\'' :: tl -> '\'' :: helper tl
      | ('a' .. 'y' as c) :: [] -> [ get_next_letter c ]
      | _ -> [ '\''; 'a' ]
    in
    TVar (charlst_to_str (helper (str_to_charlst s)))
  | _ -> TVar "'a"
;;

let rec check_occur tv tree =
  match tree with
  | TVar x -> tv = x
  | TGround _ -> false
  | TArrow (l, r) -> check_occur tv l || check_occur tv r
;;

let fresh =
  let* { sub = sb; fresh = fr } = read in
  let new_fr = get_next_tv fr in
  let* () = write { sub = sb; fresh = new_fr } in
  return fr
;;

let sub_extend tv tree =
  let* { sub = sb; fresh = fr } = read in
  let sb' = Table.extend tv tree sb in
  write { sub = sb'; fresh = fr }
;;

let sub_lookup x =
  let* { sub = sb; fresh = _ } = read in
  return (Table.lookup x sb)
;;

let rec walk ty gamma =
  match ty with
  | TVar x ->
    let res = Table.lookup x gamma in
    (match res with
     | None -> ty
     | Some y -> y)
  | TArrow (l, r) -> TArrow (walk l gamma, walk r gamma)
  | TGround _ -> ty
;;

let rec sub_walk ty =
  match ty with
  | TVar x ->
    let* look = sub_lookup x in
    (match look with
     | None -> return ty
     | Some y ->
       let* res = sub_walk y in
       return res)
  | TArrow (l, r) ->
    let* l' = sub_walk l in
    let* r' = sub_walk r in
    return (TArrow (l', r'))
  | TGround _ -> return ty
;;

let instantiate sch =
  let { vars; ty } = sch in
  let rec make_env lst env =
    match lst with
    | [] -> return env
    | h :: t ->
      let* tv = fresh in
      let* env' = make_env t env in
      return (Table.extend h tv env')
  in
  let* env = make_env vars Table.empty in
  return (walk ty env)
;;

let rec unify l r =
  match l, r with
  | TVar x, TVar y when x = y -> return ()
  | (TVar x as tv), _ ->
    if check_occur x r
    then fail (IOccursCheck (x, r))
    else
      let* look = sub_walk tv in
      (match look with
       | tv' when tv' = tv ->
         let* () = sub_extend x r in
         return ()
       | sigma_x -> unify sigma_x r)
  | _, (TVar x as tv) ->
    if check_occur x l
    then fail (IOccursCheck (x, l))
    else
      let* look = sub_walk tv in
      (match look with
       | tv' when tv' = tv ->
         let* () = sub_extend x l in
         return ()
       | sigma_x -> unify sigma_x l)
  | TArrow (x1, y1), TArrow (x2, y2) ->
    let* () = unify x1 x2 in
    unify y1 y2
  | TGround x, TGround y ->
    if x = y then return () else fail (ITypeMismatch (TGround x, TGround y))
  | _ -> fail (ITypeMismatch (l, r))
;;

let generalize env ty =
  let rec get_set env ty =
    match ty with
    | TVar x as tv ->
      (match Table.contains_value { vars = []; ty = tv } env with
       | false -> StrSet.add x StrSet.empty
       | true -> StrSet.empty)
    | TGround _ -> StrSet.empty
    | TArrow (l, r) -> StrSet.union (get_set env l) (get_set env r)
  in
  { vars = StrSet.to_list (get_set env ty); ty }
;;

let rec infer env exp =
  match exp with
  | EConst e ->
    (match e with
     | IConst _ -> return (TGround GInt)
     | FConst _ -> return (TGround GFloat)
     | BConst _ -> return (TGround GBool))
  | EVar e ->
    (match Table.lookup e env with
     | None -> fail (IUnboundValue e)
     | Some x ->
       let* inst = instantiate x in
       return inst)
  | EBinOp (op, l, r) ->
    let* l' = infer env l in
    let* r' = infer env r in
    (match op with
     | Add | Sub | Div | Mul ->
       let* () = unify l' r' in
       let* () = unify l' (TGround GInt) in
       sub_walk l'
     | Lt | Leq | Gt | Geq | Eq | Neq ->
       let* () = unify l' r' in
       let* _ = sub_walk l' in
       return (TGround GBool)
     | AddF | SubF | DivF | MulF ->
       let* () = unify l' r' in
       let* () = unify l' (TGround GFloat) in
       sub_walk l'
     | And | Or ->
       let* () = unify l' r' in
       let* () = unify l' (TGround GBool) in
       sub_walk l')
  | EFun (EVar x, body) ->
    let* tv = fresh in
    let env' = Table.extend x { vars = []; ty = tv } env in
    let* body' = infer env' body in
    sub_walk (TArrow (tv, body'))
  | EApp (l, r) ->
    let* l' = infer env l in
    let* r' = infer env r in
    let* tv = fresh in
    let* () = unify l' (TArrow (r', tv)) in
    sub_walk tv
  | EIf (cond, if_body, else_body) ->
    let* cond' = infer env cond in
    let* () = unify cond' (TGround GBool) in
    let* if_body' = infer env if_body in
    let* else_body' = infer env else_body in
    let* () = unify if_body' else_body' in
    sub_walk if_body'
  | ELet (Nonrecursive, Bind (EVar x, e1), e2) ->
    let* e1' = infer env e1 in
    let sch = generalize env e1' in
    let env' = Table.extend x sch env in
    let* e2' = infer env' e2 in
    sub_walk e2'
  | ELet (Recursive, Bind (EVar x, e1), e2) ->
    let* tv = fresh in
    let env' = Table.extend x { vars = []; ty = tv } env in
    let* e1' = infer env' e1 in
    let* () = unify e1' tv in
    let sch = generalize env' tv in
    let* e2' = infer (Table.extend x sch env') e2 in
    sub_walk e2'
  | _ -> fail ITypeError
;;

let infer_toplevel env tl =
  match tl with
  | TopExpr e ->
    let* ty = infer env e in
    let* ty' = sub_walk ty in
    return (env, RExpr ty')
  | TopLet (Nonrecursive, Bind (EVar x, e)) ->
    let* ty = infer env e in
    let* ty' = sub_walk ty in
    let sch = generalize env ty' in
    let env' = Table.extend x sch env in
    return (env', RLet (x, sch))
  | TopLet (Recursive, Bind (EVar x, e)) ->
    let* tv = fresh in
    let env' = Table.extend x { vars = []; ty = tv } env in
    let* ty = infer env' e in
    let* () = unify ty tv in
    let* ty' = sub_walk tv in
    let sch = generalize env ty' in
    let env'' = Table.extend x sch env in
    return (env'', RLet (x, sch))
  | _ -> fail ITypeError
;;

let run_infer exp env = run (infer_toplevel env exp) initial_state
