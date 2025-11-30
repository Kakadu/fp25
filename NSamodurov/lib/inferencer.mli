module Scheme : sig
  type t

  val mono : Type.ty -> Type.scheme
end

module Subst : sig
  type t

  val empty : t
  val remove : int -> t -> t
  val apply : t -> Type.ty -> Type.ty
end

module InferMonad : sig
  type ('s, 'a) t

  val return : 'a -> ('s, 'a) t
  val bind : ('s, 'a) t -> ('a -> ('s, 'b) t) -> ('s, 'b) t

  module Syntax : sig
    val ( >>= ) : ('s, 'a) t -> ('a -> ('s, 'b) t) -> ('s, 'b) t
    val ( let* ) : ('s, 'a) t -> ('a -> ('s, 'b) t) -> ('s, 'b) t
  end

  val fail : 's -> ('s, 'a) t
  val run : ('s, 'a) t -> (Subst.t * 'a, 's) Result.t
end

val unify : Type.ty -> Type.ty -> (Utils.error, unit) InferMonad.t
val w : Ast.brujin Ast.t -> (Type.ty, Utils.error) Result.t
val env : Type.scheme Type.IMap.t
