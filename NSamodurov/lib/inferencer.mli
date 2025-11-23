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

module Context : sig
  type key = int

  val add : key -> 'a -> 'a Map.Make(Int).t -> 'a Map.Make(Int).t
  val add_to_list : key -> 'a -> 'a list Map.Make(Int).t -> 'a list Map.Make(Int).t
  val update : key -> ('a option -> 'a option) -> 'a Map.Make(Int).t -> 'a Map.Make(Int).t
  val singleton : key -> 'a -> 'a Map.Make(Int).t
  val remove : key -> 'a Map.Make(Int).t -> 'a Map.Make(Int).t

  val merge
    :  (key -> 'a option -> 'b option -> 'c option)
    -> 'a Map.Make(Int).t
    -> 'b Map.Make(Int).t
    -> 'c Map.Make(Int).t

  val union
    :  (key -> 'a -> 'a -> 'a option)
    -> 'a Map.Make(Int).t
    -> 'a Map.Make(Int).t
    -> 'a Map.Make(Int).t

  val cardinal : 'a Map.Make(Int).t -> int
  val bindings : 'a Map.Make(Int).t -> (key * 'a) list
  val min_binding : 'a Map.Make(Int).t -> key * 'a
  val min_binding_opt : 'a Map.Make(Int).t -> (key * 'a) option
  val max_binding : 'a Map.Make(Int).t -> key * 'a
  val max_binding_opt : 'a Map.Make(Int).t -> (key * 'a) option
  val choose : 'a Map.Make(Int).t -> key * 'a
  val choose_opt : 'a Map.Make(Int).t -> (key * 'a) option
  val find : key -> 'a Map.Make(Int).t -> 'a
  val find_opt : key -> 'a Map.Make(Int).t -> 'a option
  val find_first : (key -> bool) -> 'a Map.Make(Int).t -> key * 'a
  val find_first_opt : (key -> bool) -> 'a Map.Make(Int).t -> (key * 'a) option
  val find_last : (key -> bool) -> 'a Map.Make(Int).t -> key * 'a
  val find_last_opt : (key -> bool) -> 'a Map.Make(Int).t -> (key * 'a) option
  val iter : (key -> 'a -> unit) -> 'a Map.Make(Int).t -> unit
  val fold : (key -> 'a -> 'acc -> 'acc) -> 'a Map.Make(Int).t -> 'acc -> 'acc
  val map : ('a -> 'b) -> 'a Map.Make(Int).t -> 'b Map.Make(Int).t
  val mapi : (key -> 'a -> 'b) -> 'a Map.Make(Int).t -> 'b Map.Make(Int).t
  val filter : (key -> 'a -> bool) -> 'a Map.Make(Int).t -> 'a Map.Make(Int).t
  val filter_map : (key -> 'a -> 'b option) -> 'a Map.Make(Int).t -> 'b Map.Make(Int).t

  val partition
    :  (key -> 'a -> bool)
    -> 'a Map.Make(Int).t
    -> 'a Map.Make(Int).t * 'a Map.Make(Int).t

  val split
    :  key
    -> 'a Map.Make(Int).t
    -> 'a Map.Make(Int).t * 'a option * 'a Map.Make(Int).t

  val is_empty : 'a Map.Make(Int).t -> bool
  val mem : key -> 'a Map.Make(Int).t -> bool
  val equal : ('a -> 'a -> bool) -> 'a Map.Make(Int).t -> 'a Map.Make(Int).t -> bool
  val compare : ('a -> 'a -> int) -> 'a Map.Make(Int).t -> 'a Map.Make(Int).t -> int
  val for_all : (key -> 'a -> bool) -> 'a Map.Make(Int).t -> bool
  val exists : (key -> 'a -> bool) -> 'a Map.Make(Int).t -> bool
  val to_list : 'a Map.Make(Int).t -> (key * 'a) list
  val of_list : (key * 'a) list -> 'a Map.Make(Int).t
  val to_seq : 'a Map.Make(Int).t -> (key * 'a) Seq.t
  val to_rev_seq : 'a Map.Make(Int).t -> (key * 'a) Seq.t
  val to_seq_from : key -> 'a Map.Make(Int).t -> (key * 'a) Seq.t
  val add_seq : (key * 'a) Seq.t -> 'a Map.Make(Int).t -> 'a Map.Make(Int).t
  val of_seq : (key * 'a) Seq.t -> 'a Map.Make(Int).t

  type t = Type.scheme Map.Make(Int).t

  val empty : 'a Map.Make(Int).t
end

val unify : Type.ty -> Type.ty -> (Utils.error, unit) InferMonad.t

val infer
  :  Type.scheme Map.Make(Int).t
  -> Ast.brujin Ast.t
  -> (Utils.error, Type.ty) InferMonad.t

val w : Ast.brujin Ast.t -> (Type.ty, Utils.error) Result.t
val env : Type.scheme Type.IMap.t
