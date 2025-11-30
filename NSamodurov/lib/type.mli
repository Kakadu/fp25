type ty =
  | TGround of string
  | TArrow of ty * ty
  | TVar of int

val tground : string -> ty
val tarrow : ty -> ty -> ty
val tvar : int -> ty
val tint : ty
val tbool : ty

module ISet : sig
  include Stdlib.Set.S with type elt := int

  val pp : Format.formatter -> t -> unit
end

module IMap : sig
  include Stdlib.Map.S with type key := int

  val pp : Format.formatter -> int t -> unit
end

val occurs_in : ty -> ty -> bool
val fv : ty -> ISet.t

type binder_set = ISet.t
type scheme = binder_set * ty

(** Pretty printer for types *)
val pp_ty : Format.formatter -> ty -> unit
