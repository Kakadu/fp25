module type MONAD_FAIL = sig
  type ('ok, 'err) t

  val return : 'ok -> ('ok, 'err) t
  val fail : 'err -> ('ok, 'err) t
  val ( >>= ) : ('a, 'err) t -> ('a -> ('b, 'err) t) -> ('b, 'err) t
  val ( let* ) : ('a, 'err) t -> ('a -> ('b, 'err) t) -> ('b, 'err) t
  val ( <*> ) : ('a -> 'b, 'err) t -> ('a, 'err) t -> ('b, 'err) t
end

module RESULT_MONAD : MONAD_FAIL with type ('ok, 'err) t = ('ok, 'err) Result.t
