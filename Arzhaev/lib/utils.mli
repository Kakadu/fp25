module type TABLE = sig
  type 'a t

  val empty : 'a t
  val extend : string -> 'a -> 'a t -> 'a t
  val lookup : string -> 'a t -> 'a option
  val contains_value : 'a -> 'a t -> bool
  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end

module Table : TABLE

val charlst_to_str : char list -> string
val str_to_charlst : string -> char list
val get_next_letter : char -> char

module StrSet : Set.S with type elt = string
