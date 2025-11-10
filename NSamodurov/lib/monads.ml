(* open Base.Monad *)

module type GENERAL_MONAD = sig
  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t

  module Syntax : sig
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end
end

module type GENERAL_MONAD_2 = sig
  type ('s, 'a) t

  val return : 'a -> ('s, 'a) t
  val bind : ('s, 'a) t -> ('a -> ('s, 'b) t) -> ('s, 'b) t

  module Syntax : sig
    val ( >>= ) : ('s, 'a) t -> ('a -> ('s, 'b) t) -> ('s, 'b) t
    val ( let* ) : ('s, 'a) t -> ('a -> ('s, 'b) t) -> ('s, 'b) t
  end
end

module type GENERAL_MONAD_3 = sig
  type ('s, 'e, 'a) t

  val return : 'a -> ('s, 'e, 'a) t
  val bind : ('s, 'e, 'a) t -> ('a -> ('s, 'e, 'b) t) -> ('s, 'e, 'b) t

  module Syntax : sig
    val ( >>= ) : ('s, 'e, 'a) t -> ('a -> ('s, 'e, 'b) t) -> ('s, 'e, 'b) t
    val ( let* ) : ('s, 'e, 'a) t -> ('a -> ('s, 'e, 'b) t) -> ('s, 'e, 'b) t
  end
end

module type STATE_MONAD = sig
  include GENERAL_MONAD_2

  val read : ('s, 's) t
  val run : ('s, 'a) t -> 's -> 'a
  val write : 's -> ('s, unit) t
end
