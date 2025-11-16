module type STATE_MONAD = sig
  type ('s, 'ok, 'err) t

  val return : 'ok -> ('s, 'ok, 'err) t
  val fail : 'err -> ('s, 'ok, 'err) t
  val ( >>= ) : ('s, 'a, 'e) t -> ('a -> ('s, 'b, 'e) t) -> ('s, 'b, 'e) t
  val ( let* ) : ('s, 'a, 'e) t -> ('a -> ('s, 'b, 'e) t) -> ('s, 'b, 'e) t
  val ( <*> ) : ('s, 'a -> 'b, 'e) t -> ('s, 'a, 'e) t -> ('s, 'b, 'e) t
  val get : ('s, 's, 'e) t
  val put : 's -> ('s, unit, 'e) t
  val fresh_int : (int, int, 'err) t
  val run : ('s, 'ok, 'err) t -> 's -> ('s * 'ok, 'err) Result.t
end

module State : STATE_MONAD = struct
  type ('s, 'ok, 'err) t = 's -> ('s * 'ok, 'err) Result.t

  let get s = Ok (s, s)
  let put new_s _ = Ok (new_s, ())
  let return x s = Ok (s, x)
  let fail msg _ = Error msg

  let ( >>= ) m f s =
    match m s with
    | Error e -> Error e
    | Ok (s', v) -> f v s'
  ;;

  let ( let* ) = ( >>= )
  let ( <*> ) mf mx = mf >>= fun f -> mx >>= fun x -> return (f x)
  let fresh_int s = Ok (s + 1, s)
  let run m s = m s
end
