(* open Base.Monad *)

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

module type FailMonad = sig
  include GENERAL_MONAD_2

  val fail : 'e -> ('e, 'a) t
end

(** Monad for global context  *)
module Env : STATE_MONAD = struct
  type ('s, 'a) t = 's -> 's * 'a

  let return : 'a -> ('s, 'a) t = fun x s -> s, x

  let bind : ('s, 'a) t -> ('a -> ('s, 'b) t) -> ('s, 'b) t =
    fun m f st ->
    let st, x = m st in
    f x st
  ;;

  module Syntax = struct
    let ( let* ) = bind
    let ( >>= ) = bind
  end

  let read : ('s, 's) t = fun st -> st, st
  let run : ('s, 'a) t -> 's -> 'a = fun f st -> snd (f st)
  let write : 's -> ('s, unit) t = fun x _ -> x, ()
end
