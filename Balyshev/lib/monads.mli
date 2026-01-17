module type STATE_MONAD = sig
  type ('s, 'ok, 'err) t

  val return : 'ok -> ('s, 'ok, 'err) t
  val fail : 'err -> ('s, 'ok, 'err) t
  val ( >>= ) : ('s, 'a, 'err) t -> ('a -> ('s, 'b, 'err) t) -> ('s, 'b, 'err) t
  val ( let* ) : ('s, 'a, 'err) t -> ('a -> ('s, 'b, 'err) t) -> ('s, 'b, 'err) t
  val ( <*> ) : ('s, 'a -> 'b, 'err) t -> ('s, 'a, 'err) t -> ('s, 'b, 'err) t
  val ( <|> ) : ('s, 'a, 'err) t -> ('s, 'a, 'err) t -> ('s, 'a, 'err) t
  val get : ('s, 's, 'err) t
  val put : 's -> ('s, unit, 'err) t
  val run : ('s, 'ok, 'err) t -> 's -> ('s * 'ok, 'err) Result.t
end

(* implementation of STATE_MONAD based on Stdlib.Result *)
module State : STATE_MONAD
