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
  | TVar of tvar ref

and tvar =
  | Unbound of int
  | Link of typ

let fresh_tyvar =
  let counter = ref 0 in
  fun () ->
    incr counter;
    TVar (ref (Unbound !counter))
;;

type scheme = Forall of int list * typ
type type_env = (string * scheme) list

let rec look_up x = function
  | [] -> None
  | (y, v) :: _ when String.equal x y -> Some v
  | _ :: tl -> look_up x tl
;;

exception TypeError of string

(* Раскрытие ссылок *)
let rec prune = function
  | TVar { contents = Link t } ->
    let t' = prune t in
    (match t with
     | TVar r -> r := Link t'
     | _ -> ());
    t'
  | t -> t
;;

(* Occurs check: проверка на рекурсивные типы *)
let rec occurs tv = function
  | TInt | TBool | TUnit -> false
  | TFun (t1, t2) -> occurs tv t1 || occurs tv t2
  | TTuple ts -> List.exists (occurs tv) ts (* Проверяем внутри кортежа *)
  | TVar r ->
    (match !r with
     | Unbound id -> id = tv
     | Link t -> occurs tv t)
;;

(* Унификация *)
let rec unify t1 t2 =
  let t1 = prune t1 in
  let t2 = prune t2 in
  match t1, t2 with
  | TInt, TInt | TBool, TBool | TUnit, TUnit -> ()
  | TFun (a1, r1), TFun (a2, r2) ->
    unify a1 a2;
    unify r1 r2
  | TTuple ts1, TTuple ts2 ->
    if List.length ts1 <> List.length ts2 then raise (TypeError "tuple length mismatch");
    List.iter2 unify ts1 ts2
  | TVar ({ contents = Unbound id } as r), t | t, TVar ({ contents = Unbound id } as r) ->
    if occurs id t then raise (TypeError "recursive type") else r := Link t
  | _ -> raise (TypeError "type mismatch")
;;

(* Свободные типовые переменные *)
module IntSet = Set.Make (Int)

let rec free_tyvars = function
  | TInt | TBool | TUnit -> IntSet.empty
  | TFun (t1, t2) -> IntSet.union (free_tyvars t1) (free_tyvars t2)
  | TTuple ts ->
    List.fold_left (fun acc t -> IntSet.union acc (free_tyvars t)) IntSet.empty ts
  | TVar { contents = Unbound id } -> IntSet.singleton id
  | TVar { contents = Link t } -> free_tyvars t
;;

(* Обобщение *)
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

(* Инстанцирование *)
let instantiate (Forall (vars, t)) =
  let subst = List.fold_left (fun acc id -> (id, fresh_tyvar ()) :: acc) [] vars in
  let rec go = function
    | (TInt | TBool | TUnit) as t -> t
    | TFun (t1, t2) -> TFun (go t1, go t2)
    | TTuple ts -> TTuple (List.map go ts)
    | TVar { contents = Unbound id } ->
      (match List.assoc_opt id subst with
       | Some t -> t
       | None -> fresh_tyvar ())
    | TVar { contents = Link t } -> go t
  in
  go t
;;

[ "print_int", Forall ([], TFun (TInt, TUnit))
; "print_bool", Forall ([], TFun (TBool, TUnit))
; "true", Forall ([], TBool)
; "false", Forall ([], TBool)
]

(* Вспомогательная функция для извлечения переменных из паттерна и их типов *)
let rec bind_pattern p t =
  match p, prune t with
  | PVar x, t -> [ x, t ]
  | PTuple ps, TTuple ts ->
    if List.length ps <> List.length ts
    then raise (TypeError "tuple pattern length mismatch");
    List.concat (List.map2 bind_pattern ps ts)
  | PTuple ps, TVar ({ contents = Unbound _ } as r) ->
    (* Если паттерн — кортеж, а тип — неизвестная переменная,
       значит эта переменная ОБЯЗАНА быть кортежем *)
    let element_tys = List.map (fun _ -> fresh_tyvar ()) ps in
    let tuple_ty = TTuple element_tys in
    r := Link tuple_ty;
    List.concat (List.map2 bind_pattern ps element_tys)
  | _ -> raise (TypeError "pattern matching error: type mismatch")
;;

(* Основная функция infer *)
let rec infer env e =
  match e with
  | Int _ -> TInt
  | Bool _ -> TBool
  | Var x ->
    (match List.assoc_opt x env with
     | Some scheme -> instantiate scheme
     | None -> raise (TypeError ("unbound variable " ^ x)))
  | Abs (pat, body) ->
    let tv = fresh_tyvar () in
    let bindings = bind_pattern pat tv in
    let env' =
      List.fold_left (fun acc (x, t) -> (x, Forall ([], t)) :: acc) env bindings
    in
    let body_ty = infer env' body in
    TFun (tv, body_ty)
  | App (f, arg) ->
    let tf = infer env f in
    let ta = infer env arg in
    let tr = fresh_tyvar () in
    unify tf (TFun (ta, tr));
    tr
  | Tuple es -> TTuple (List.map (infer env) es)
  | Let (NonRec, pat, e1, body_opt) ->
    let t1 = infer env e1 in
    let bindings = bind_pattern pat t1 in
    let env' =
      List.fold_left (fun acc (x, t) -> (x, generalize env t) :: acc) env bindings
    in
    (match body_opt with
     | Some e2 -> infer env' e2
     | None -> TUnit)
  | Let (Rec, PVar x, e1, body_opt) ->
    let tv = fresh_tyvar () in
    let env_pre = (x, Forall ([], tv)) :: env in
    let t1 = infer env_pre e1 in
    unify tv t1;
    let scheme = generalize env t1 in
    let env' = (x, scheme) :: env in
    (match body_opt with
     | Some e2 -> infer env' e2
     | None -> TUnit)
  | Let (Rec, _, _, _) ->
    raise (TypeError "recursive let only supports simple variables, not patterns")
  | If (c, t, e) ->
    let tc = infer env c in
    unify tc TBool;
    let tt = infer env t in
    let te = infer env e in
    unify tt te;
    tt
  | BinOp (op, e1, e2) ->
    let t1 = infer env e1 in
    let t2 = infer env e2 in
    (match op with
     | Plus | Minus | Mult | Div ->
       unify t1 TInt;
       unify t2 TInt;
       TInt
     | Equal | NotEqual | More | Less | EMore | ELess ->
       unify t1 TInt;
       unify t2 TInt;
       TBool
     | And | Or ->
       unify t1 TBool;
       unify t2 TBool;
       TBool)
  | UnOp (op, e) ->
    let t = infer env e in
    (match op with
     | Neg ->
       unify t TInt;
       TInt
     | Not ->
       unify t TBool;
       TBool)
;;

(* let rec type_equal t1 t2 =
   match t1, t2 with
   | TInt, TInt -> true
   | TBool, TBool -> true
   | TUnit, TUnit -> true
   | TFun (a1, r1), TFun (a2, r2) -> type_equal a1 a2 && type_equal r1 r2
   | _ -> false
   ;;

   let rec type_of env e =
   match e with
   | Int _ -> Ok TInt
   | Bool _ -> Ok TBool
   | Var x ->
   (match look_up x env with
   | Some t -> Ok t
   | None -> Error (UnboundVariable x))
   | UnOp (Neg, e) ->
   let* t = type_of env e in
   (match t with
   | TInt -> Ok TInt
   | _ -> Error (Mismatch (TInt, t)))
   | UnOp (Not, e) ->
   let* t = type_of env e in
   (match t with
   | TBool -> Ok TBool
   | _ -> Error (Mismatch (TBool, t)))
   | BinOp (op, e1, e2) ->
   let* t1 = type_of env e1 in
   let* t2 = type_of env e2 in
   (match op, t1, t2 with
   | (Plus | Minus | Div | Mult), TInt, TInt -> Ok TInt
   | (ELess | EMore | Equal | NotEqual | More | Less), TInt, TInt -> Ok TBool
   | (And | Or), TBool, TBool -> Ok TBool
   | (Plus | Minus | Div | Mult), _, _ -> Error (Mismatch (TInt, t1))
   | (ELess | EMore | Equal | NotEqual | More | Less), _, _ ->
   Error (Mismatch (TInt, t1))
   | (And | Or), _, _ -> Error (Mismatch (TBool, t1)))
   | If (cond, t, e) ->
   let* tc = type_of env cond in
   (match tc with
   | TBool ->
   let* tt = type_of env t in
   (match e with
   | None -> Ok TUnit
   | Some e1 ->
   let* te = type_of env e1 in
   (match type_equal tt te with
   | true -> Ok tt
   | false -> Error (BranchesHaveDifferentTypes (tt, te))))
   | _ -> Error (IfConditionNotBool tc))
   | _ -> Ok TInt
   ;; *)
