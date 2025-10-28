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

module type STATE_MONAD = sig
  type ('ok, 'err) t = int -> (int * 'ok, 'err) Result.t

  val fresh_int : (int, 'err) t
  val fresh_str : (string, 'err) t
  val return : 'ok -> ('ok, 'err) t
  val fail : 'err -> ('ok, 'err) t
  val run : ('ok, 'err) t -> (int * 'ok, 'err) Result.t
  val ( >>= ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
  val ( let* ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
  val ( <*> ) : ('a -> 'b, 'e) t -> ('a, 'e) t -> ('b, 'e) t
end

module State : STATE_MONAD with type ('ok, 'err) t = int -> (int * 'ok, 'err) Result.t =
struct
  type ('ok, 'err) t = int -> (int * 'ok, 'err) Result.t

  let fresh_int = fun s -> Ok (s + 1, s)
  let fresh_str = fun s -> Ok (s + 1, "'gen_" ^ Int.to_string s)
  let return x = fun s -> Ok (s, x)
  let fail msg = fun _ -> Error msg
  let run m = m 0

  let ( >>= ) (m : ('a, 'e) t) (f : 'a -> ('b, 'e) t) : ('b, 'e) t =
    fun s ->
    match m s with
    | Error e -> Error e
    | Ok (s', v) -> f v s'
  ;;

  let ( let* ) = ( >>= )
  let ( <*> ) mf mx = mf >>= fun f -> mx >>= fun x -> return (f x)
end
