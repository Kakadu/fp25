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

  let ( <|> ) m1 m2 s =
    match m1 s with
    | Ok x -> Ok x
    | Error _ -> m2 s
  ;;

  let run m s = m s
end
