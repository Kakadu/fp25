open Type
open Ast
open Monads

type error =
  [ `ParsingError of string
  | `OccursCheck of ty * ty
  | `UnifyError of ty * ty
  | `UnboundVariable of int
  | `AbstractionExpected of ty
  | `UsingReservedVariable of int
  | `ReservedError
  | `InterpretError of string
  ]
[@@deriving show { with_path = false }]

let pp_error ppf = function
  | `ParsingError s -> Format.fprintf ppf "%s" s
  | `OccursCheck (a, b) -> Format.fprintf ppf "Occurs error: %a %a" pp_ty a pp_ty b
  | `UnifyError (a, b) -> Format.fprintf ppf "Unification error: %a %a" pp_ty a pp_ty b
  | `UnboundVariable i -> Format.fprintf ppf "Unbound variable: %d" i
  | `AbstractionExpected t -> Format.fprintf ppf "AbstractionExpected: %a" pp_ty t
  | `UsingReservedVariable i -> Format.fprintf ppf "UsingReservedVariable: %d" i
  | `ReservedError -> Format.fprintf ppf "Reserved variable limit exceeded"
  | `InterpretError s -> Format.fprintf ppf "Can't interpret: %s\n" s
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

(* Triangular substituion *)
module Subst : sig
  type t

  val empty : t
  val remove : int -> t -> t
  val apply : t -> ty -> ty
  val extend : t -> int -> ty -> t
end = struct
  type t = ty IMap.t

  let empty = IMap.empty
  let remove = IMap.remove
  let extend m i ty = IMap.add i ty m

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
  let run m = snd (m (Subst.empty, reserved))
  let extend = fun i ty (sub, st) -> st, Result.ok (Subst.extend sub i ty, ())

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
    let* () = extend v t2 in
    return ()
  | _, TVar v ->
    let* () = extend v t1 in
    return ()
  | TArrow (l1, l2), TArrow (r1, r2) ->
    let* _ = unify l1 r1 in
    let* _ = unify l2 r2 in
    return ()
  | _ -> fail (`UnifyError (t1, t2))
;;

let infer env =
  let rec helper =
    fun env height -> function
      | EConst (Int _) -> return tint
      | EConst (Bool _) -> return tbool
      | EVar (Index b) when b >= reserved ->
        Context.lookup (reserved + height - b - 1) env
      | EVar (Index b) -> Context.lookup b env
      | ELet (NotRecursive, Index v, e1, e2) ->
        let* t1 = helper env height e1 in
        let t2 = gen env t1 in
        let env = Context.add v t2 env in
        let* t3 = helper env (height + 1) e2 in
        walk t3
      | ELet (Recursive, Index v, e1, e2) ->
        let* fresh = fresh in
        let tv = tvar fresh in
        let env = Context.add v (Scheme.mono tv) env in
        let* t1 = helper env height e1 in
        let* _ = unify tv t1 in
        let t2 = gen env tv in
        let* t2 = helper (Context.add v t2 env) (height + 1) e2 in
        walk t2
      | EIf (pred, e1, e2) ->
        let* pred = helper env height pred in
        let* _ = unify pred tbool in
        let* e1 = helper env height e1 in
        let* e2 = helper env height e2 in
        let* _ = unify e1 e2 in
        walk e1
      | EAbs (Index v, e) ->
        let* fresh = fresh in
        let tv = tvar fresh in
        (* List.iter (fun (k, v) -> Format.printf "%d\n" k) (Context.to_list env); *)
        let env = Context.add v (Scheme.mono tv) env in
        let* te = helper env (height + 1) e in
        walk (tarrow tv te)
      | EApp (e1, e2) ->
        let* e1 = helper env height e1 in
        let* e2 = helper env height e2 in
        (match e1 with
         | TArrow (l, r) ->
           let* _ = unify l e2 in
           walk r
         | _ -> fail (`AbstractionExpected e1))
  in
  helper env reserved
;;

let env : scheme IMap.t =
  let arith_ty = tarrow tint (tarrow tint tint) in
  let bool_ty = tarrow tbool (tarrow tbool tbool) in
  Context.empty
  |> Context.add 0 (Scheme.mono arith_ty)
  |> Context.add 1 (Scheme.mono arith_ty)
  |> Context.add 2 (Scheme.mono arith_ty)
  |> Context.add 3 (Scheme.mono arith_ty)
  |> Context.add 4 (Scheme.mono bool_ty)
  |> Context.add 5 (Scheme.mono bool_ty)
  |> Context.add 6 (Scheme.mono bool_ty)
  |> Context.add 7 (Scheme.mono bool_ty)
  |> Context.add 8 (Scheme.mono bool_ty)
  |> Context.add 9 (Scheme.mono bool_ty)
  |> Context.add 10 (Scheme.mono bool_ty)
  |> Context.add 11 (Scheme.mono bool_ty)
  |> Context.add 12 (Scheme.mono bool_ty)
;;

let w : Ast.brujin Ast.t -> (ty, error) Result.t =
  let ( let* ) = Result.bind in
  fun x ->
    if Context.cardinal env <= reserved
    then
      let* _, t = run (infer env x) in
      Result.ok t
    else Result.Error `ReservedError
;;
