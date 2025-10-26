module type MONAD_FAIL = sig
  type ('ok, 'err) t

  val return : 'ok -> ('ok, 'err) t
  val fail : 'err -> ('ok, 'err) t
  val ( >>= ) : ('a, 'err) t -> ('a -> ('b, 'err) t) -> ('b, 'err) t
  val ( let* ) : ('a, 'err) t -> ('a -> ('b, 'err) t) -> ('b, 'err) t
  val ( <*> ) : ('a -> 'b, 'err) t -> ('a, 'err) t -> ('b, 'err) t
end

module RESULT_MONAD : MONAD_FAIL with type ('ok, 'err) t = ('ok, 'err) Result.t = struct
  type ('ok, 'err) t = ('ok, 'err) Result.t

  let return (v : 'ok) : ('ok, 'err) t = Ok v
  let fail (e : 'err) : ('ok, 'err) t = Error e

  let ( >>= ) (m : ('a, 'err) t) (f : 'a -> ('b, 'err) t) : ('b, 'err) t =
    match m with
    | Ok x -> f x
    | Error e -> Error e
  ;;

  let ( let* ) = ( >>= )
  let ( <*> ) mf mx = mf >>= fun f -> mx >>= fun x -> return (f x)
end
