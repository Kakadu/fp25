open Type
open Ast
open Monads
(* 135 ml implementation:  tapl *)

type term =
  | TInt of int
  | TBool of bool
  | TVar of int
  | TAbs of term
  | TApp of term * term
[@@deriving show { with_path = false }]

type error =
  [ `Parsing_error of string
  | `UnifyError of ty * ty
  | `UnboundVariable of int
  | `AbstractionExpected of ty
  | `UsingReservedVariable of int
  | `ReservedError
  ]
[@@deriving show { with_path = false }]

let pp_error ppf = function
  | `Parsing_error s -> Format.fprintf ppf "%s" s
  | `UnifyError (a, b) -> Format.fprintf ppf "Unification error: %a %a" pp_ty a pp_ty b
  | `UnboundVariable i -> Format.fprintf ppf "Unbound variable: %d" i
  | `AbstractionExpected t -> Format.fprintf ppf "AbstractionExpected: %a" pp_ty t
  | `UsingReservedVariable i -> Format.fprintf ppf "UsingReservedVariable: %d" i
  | `ReservedError -> Format.fprintf ppf "Reserved variable limit exceeded"
;;

module Scheme : sig
  type t

  val mono : ty -> scheme
  val fv : t -> binder_set
end = struct
  type t = scheme

  let mono t = ISet.empty, t
  let fv (b, ty) = ISet.diff (Type.fv ty) b
end

module Subst : sig
  type t

  val empty : t
  val remove : int -> t -> t
  val apply : t -> ty -> ty
end = struct
  type t = ty IMap.t

  let empty = IMap.empty
  let remove = IMap.remove

  let apply map =
    let rec helper = function
      | TArrow (e1, e2) -> TArrow (helper e1, helper e2)
      | TGround _ as tg -> tg
      | TVar n ->
        (match IMap.find_opt n map with
         | Some x -> x
         | None -> TVar n)
    in
    helper
  ;;
end

module InferMonad : sig
  include GENERAL_MONAD_2

  val fail : 's -> ('s, 'a) t
  val run : ('s, 'a) t -> (Subst.t * 'a, 's) Result.t
  val fresh : ('s, int) t
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
  let current = fun (sub, st) -> st, Result.ok (sub, st)
  let run m = snd (m (Subst.empty, reserved))

  module Syntax = struct
    let ( let* ) = bind
    let ( >>= ) = bind
  end
end

open InferMonad
open InferMonad.Syntax

module Context = struct
  include IMap

  type t = scheme IMap.t

  let empty = IMap.empty
  let fv = fun m -> fold (fun _ a acc -> ISet.union acc (fst a)) m ISet.empty

  let lookup =
    fun k ctx ->
    match IMap.find_opt k ctx with
    | Some s -> return (snd s)
    | None -> fail (`UnboundVariable k)
  ;;
end

let gen =
  fun env ty ->
  let free = ISet.diff (Type.fv ty) (Context.fv env) in
  free, ty
;;

let rec unify e1 e2 =
  match e1, e2 with
  | TGround g1, TGround g2 when g1 = g2 -> return (e1, e2)
  | TVar b1, TVar b2 when b1 = b2 -> return (e1, e2)
  | TGround _, TGround _ -> fail (`UnifyError (e1, e2))
  | TArrow (l1, l2), TArrow (r1, r2) ->
    let* l1, r1 = unify l1 r1 in
    let* l2, r2 = unify l2 r2 in
    return (tarrow l1 r1, tarrow l2 r2)
  | _ -> fail (`UnifyError (e1, e2))
;;

let infer env =
  let rec helper =
    fun env height -> function
      | EConst (Int _) -> return tint
      | EVar (Index b) when b >= reserved ->
        Context.lookup (reserved + height - b - 1) env
      | EVar (Index b) -> Context.lookup b env
      | ELet (NotRecursive, Index v, e1, e2) ->
        let* t1 = helper env height e1 in
        let t2 = gen env t1 in
        let env = Context.add v t2 env in
        let* t3 = helper env (height + 1) e2 in
        return t3
      | ELet (Recursive, Index v, e1, e2) ->
        let* fresh = fresh in
        let tv = tvar fresh in
        let env = Context.add v (Scheme.mono tv) env in
        let* t1 = helper env height e1 in
        let* _ = unify tv t1 in
        let t2 = gen env tv in
        let* t2 = helper (Context.add v t2 env) (height + 1) e2 in
        return t2
      | EAbs (Index v, e) ->
        let* fresh = fresh in
        let tv = tvar fresh in
        (* List.iter (fun (k, v) -> Format.printf "%d\n" k) (Context.to_list env); *)
        let env = Context.add v (Scheme.mono tv) env in
        let* te = helper env (height + 1) e in
        return (tarrow tv te)
      | EApp (e1, e2) ->
        let* e1 = helper env height e1 in
        let* e2 = helper env height e2 in
        (match e1 with
         | TArrow (l, r) ->
           let* _ = unify l e2 in
           return r
         | _ -> fail (`AbstractionExpected e1))
  in
  helper env reserved
;;

let env : scheme IMap.t =
  let arith_ty = tarrow tint (tarrow tint tint) in
  Context.empty
  |> Context.add 0 (Scheme.mono arith_ty)
  |> Context.add 1 (Scheme.mono arith_ty)
  |> Context.add 2 (Scheme.mono arith_ty)
  |> Context.add 3 (Scheme.mono arith_ty)
;;

let w : Ast.brujin Ast.t -> (error, Type.ty) InferMonad.t =
  fun x -> if Context.cardinal env != reserved then infer env x else fail `ReservedError
;;
