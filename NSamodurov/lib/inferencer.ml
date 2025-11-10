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
  [ `UnifyError of ty * ty
  | `UnboundVariable of int
  | `AbstractionExpected of ty
  | `UsingReservedVariable of int
  ]

(* Gamma *)
module Scheme : sig
  type t

  val mono : ty -> scheme
end = struct
  type t = scheme

  let mono t = ISet.empty, t
  let fv (b, ty) = ISet.diff (Type.fv ty) b
end

module InferMonad : sig
  include GENERAL_MONAD_2

  val fail : 's -> ('s, 'a) t
  val to_result : ('s, 'a) t -> ('a, 's) Result.t
end = struct
  type ('s, 'a) t = ('a, 's) Result.t

  let fail = Result.error
  let return = Result.ok
  let bind = Result.bind

  let to_result = function
    | Ok _ as o -> o
    | Error _ as e -> e
  ;;

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

  let lookup =
    fun k ctx ->
    match IMap.find_opt k ctx with
    | Some s -> return (snd s)
    | None -> fail (`UnboundVariable k)
  ;;
end

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
    fun env -> function
      | EConst (Int _) -> return tint
      | EVar (Index b) -> Context.lookup b env
      | EVar Blank -> fail (`UsingReservedVariable (-1))
      | ELet (NotRecursive, Index v, e1, e2) ->
        let tv = tvar v in
        let env = Context.add v (Scheme.mono tv) env in
        failwith "need to implement generalize"
      | ELet (Recursive, Index v, e1, e2) -> failwith "unimpl let"
      | ELet (_, Blank, e1, e2) -> failwith "TODO: remove blanks"
      | EAbs (Index v, e) ->
        let tv = tvar v in
        let env = Context.add v (Scheme.mono tv) env in
        let* te = helper env e in
        return (tarrow tv te)
      | EAbs (Blank, e) -> failwith "unimpl abs"
      | EApp (e1, e2) ->
        let* e1 = helper env e1 in
        let* e2 = helper env e2 in
        (match e1 with
         | TArrow (l, r) ->
           let* _ = unify l e2 in
           return r
         | _ -> fail (`AbstractionExpected e1))
  in
  helper env
;;

let default_env =
  let arith_ty = tarrow tint (tarrow tint tint) in
  Context.empty
  |> Context.add 0 (Scheme.mono arith_ty)
  |> Context.add 1 (Scheme.mono arith_ty)
  |> Context.add 2 (Scheme.mono arith_ty)
  |> Context.add 3 (Scheme.mono arith_ty)
;;

let w x = infer default_env x
