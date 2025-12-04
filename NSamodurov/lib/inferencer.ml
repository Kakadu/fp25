[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Type
open Ast
open Monads

type error =
  [ `OccursCheck of ty * ty
  | `UnifyError of ty * ty
  | `UnboundVariable of name * int
  ]
[@@deriving show { with_path = false }]

module Scheme : sig
  type t

  val mono : ty -> scheme
end = struct
  type t = scheme

  let mono t = ISet.empty, t
end

(* Triangular substituion *)
module Subst : sig
  type t

  val empty : t
  val remove : int -> t -> t
  val apply : t -> ty -> ty
  val extend : t -> int -> ty -> t
  val print : t -> unit
end = struct
  type t = ty IMap.t

  let empty = IMap.empty

  let print map =
    List.iter
      (fun (i, a) -> Format.printf "(%d, %a)" i pp_ty a)
      (List.of_seq (IMap.to_seq map));
    Format.printf "\n"
  ;;

  let remove = IMap.remove
  let extend m i ty = IMap.add i ty m

  let apply map =
    (* IMap.iter (fun k v -> Format.printf "%d: %a\n" k pp_ty v) map; *)
    let rec helper = function
      | TArrow (e1, e2) -> TArrow (helper e1, helper e2)
      | TGround _ as tg -> tg
      | TVar n as tv ->
        (match IMap.find_opt n map with
         | Some x -> x
         | None -> tv)
    in
    helper
  ;;
end

module InferMonad : sig
  include GENERAL_MONAD_2

  val fail : 's -> ('s, 'a) t
  val run : ('s, 'a) t -> (Subst.t * 'a, 's) Result.t
  val fresh : ('s, int) t
  val subst : ('s, Subst.t) t
  val extend : int -> ty -> ('s, unit) t
end = struct
  type ('s, 'a) t = Subst.t * int -> int * (Subst.t * 'a, 's) Result.t

  let fail e (_, st) = st, Result.error e
  let return x (sub, st) = st, Result.ok (sub, x)

  let bind =
    fun o f st ->
    let last, r = o st in
    match r with
    | Result.Error _ as e -> last, e
    | Ok (sub, v) -> (f v) (sub, last)
  ;;

  let fresh = fun (sub, st) -> st + 1, Result.ok (sub, st)
  let subst = fun (sub, st) -> st, Result.ok (sub, sub)
  let run m = snd (m (Subst.empty, 0))
  let extend i ty (sub, st) = st, Result.ok (Subst.extend sub i ty, ())

  module Syntax = struct
    let ( let* ) = bind
    let ( >>= ) = bind
  end
end

open InferMonad
open InferMonad.Syntax

module Context = struct
  include IMap

  let print map =
    List.iter
      (fun (i, (_, a)) -> if i >= 0 then Format.printf "(%d, %a)" i pp_ty a)
      (List.of_seq (IMap.to_seq map));
    Format.printf "\n"
  ;;

  let fv = fun m -> fold (fun _ a acc -> ISet.union acc (fst a)) m ISet.empty
end

let inst =
  fun scheme ->
  let binder, ty = scheme in
  let fold f =
    ISet.fold (fun x acc ->
      let* acc = acc in
      f acc x)
  in
  let* sub =
    fold
      (fun acc x ->
         let* fresh = fresh in
         let tv = tvar fresh in
         return (Subst.extend acc x tv))
      binder
      (return Subst.empty)
  in
  return (Subst.apply sub ty)
;;

let lookup =
  fun (n, k) ctx ->
  match IMap.find_opt k ctx with
  | Some s -> inst s
  | None -> fail (`UnboundVariable (n, k))
;;

let gen =
  fun env ty ->
  let free = ISet.diff (Type.fv ty) (Context.fv env) in
  free, ty
;;

let walk =
  fun ty ->
  let* sub = subst in
  let ty = Subst.apply sub ty in
  return ty
;;

let rec unify t1 t2 =
  match t1, t2 with
  | TGround g1, TGround g2 when g1 = g2 -> return ()
  | TVar b1, TVar b2 when b1 = b2 -> return ()
  | TVar _, _ when Type.occurs_in t1 t2 -> fail (`OccursCheck (t1, t2))
  | TVar v, _ ->
    let* _ = extend v t2 in
    return ()
  | _, TVar v ->
    let* _ = extend v t1 in
    return ()
  | TArrow (l1, r1), TArrow (l2, r2) ->
    let* _ = unify l1 l2 in
    let* r1 = walk r1 in
    let* r2 = walk r2 in
    let* _ = unify r1 r2 in
    return ()
  | _ -> fail (`UnifyError (t1, t2))
;;

let list_of_apps =
  let rec helper = function
    | EApp (a1, a2) -> a2 :: helper a1
    | a -> [ a ]
  in
  fun l -> List.rev (helper l)
;;

let infer env =
  let rec helper =
    fun env height -> function
      | EConst (Int _) -> return tint
      | EConst (Bool _) -> return tbool
      | EVar (Index (v, b)) when b >= 0 ->
        assert (height - b - 1 >= 0);
        lookup (v, height - b - 1) env
      | EVar (Index (v, b)) -> lookup (v, b) env
      | ELet (NotRecursive, Index (_, v), e1, e2) ->
        let* t1 = helper env height e1 in
        let t2 = gen env t1 in
        let env = Context.add v t2 env in
        let* t3 = helper env (height + 1) e2 in
        walk t3
      | ELet (Recursive, Index (_, v), e1, e2) ->
        let* fresh = fresh in
        let tv = tvar fresh in
        let env = Context.add v (Scheme.mono tv) env in
        let* t1 = helper env (height + 1) e1 in
        let* _ = unify tv t1 in
        let t2 = gen env tv in
        let* t2 = helper (Context.add v t2 env) (height + 1) e2 in
        walk t2
      | EIf (pred, e1, e2) ->
        let* pred = helper env height pred in
        let* _ = unify pred tbool in
        let* t1 = helper env height e1 in
        let* t2 = helper env height e2 in
        let* _ = unify t1 t2 in
        walk t1
      | EAbs (Index (_, v), e) ->
        let* fresh = fresh in
        let tv = tvar fresh in
        let env = Context.add v (Scheme.mono tv) env in
        let* te = helper env (height + 1) e in
        walk (tarrow tv te)
      | EApp (e1, e2) ->
        let* t1 = helper env height e1 in
        let* t2 = helper env height e2 in
        let* fresh = fresh in
        let tv = tvar fresh in
        let* _ = unify t1 (tarrow t2 tv) in
        walk tv
  in
  helper env 0
;;

let env : scheme IMap.t =
  let arith_ty = tarrow tint (tarrow tint tint) in
  let bool_ty = tarrow tbool (tarrow tbool tbool) in
  let cmp_ty = tarrow tint (tarrow tint tbool) in
  Context.empty
  |> Context.add (-1) (Scheme.mono arith_ty)
  |> Context.add (-2) (Scheme.mono arith_ty)
  |> Context.add (-3) (Scheme.mono arith_ty)
  |> Context.add (-4) (Scheme.mono arith_ty)
  |> Context.add (-5) (Scheme.mono cmp_ty)
  |> Context.add (-6) (Scheme.mono cmp_ty)
  |> Context.add (-7) (Scheme.mono cmp_ty)
  |> Context.add (-8) (Scheme.mono cmp_ty)
  |> Context.add (-9) (Scheme.mono cmp_ty)
  |> Context.add (-10) (Scheme.mono cmp_ty)
  |> Context.add (-11) (Scheme.mono cmp_ty)
  |> Context.add (-12) (Scheme.mono bool_ty)
  |> Context.add (-13) (Scheme.mono bool_ty)
  |> Context.add (-14) (Scheme.mono (tarrow tint tint))
;;

let w =
  let ( let* ) = Result.bind in
  fun x ->
    let* _, t = run (infer env x) in
    Result.ok t
;;
