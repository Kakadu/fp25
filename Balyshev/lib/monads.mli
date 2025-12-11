module type STATE_MONAD = sig
  type ('s, 'ok, 'err) t

  val return : 'ok -> ('s, 'ok, 'err) t
  val fail : 'err -> ('s, 'ok, 'err) t
  val ( >>= ) : ('s, 'a, 'e) t -> ('a -> ('s, 'b, 'e) t) -> ('s, 'b, 'e) t
  val ( let* ) : ('s, 'a, 'e) t -> ('a -> ('s, 'b, 'e) t) -> ('s, 'b, 'e) t
  val ( <*> ) : ('s, 'a -> 'b, 'e) t -> ('s, 'a, 'e) t -> ('s, 'b, 'e) t
  val get : ('s, 's, 'e) t
  val put : 's -> ('s, unit, 'e) t
  val run : ('s, 'ok, 'err) t -> 's -> ('s * 'ok, 'err) Result.t
end

(* implementation of STATE_MONAD based on Stdlib.Result *)
module State : STATE_MONAD
