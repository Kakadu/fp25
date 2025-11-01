module type MONAD_FAIL = sig
  type ('ok, 'err) t

  val return : 'ok -> ('ok, 'err) t
  val fail : 'err -> ('ok, 'err) t
  val ( >>= ) : ('a, 'err) t -> ('a -> ('b, 'err) t) -> ('b, 'err) t
  val ( let* ) : ('a, 'err) t -> ('a -> ('b, 'err) t) -> ('b, 'err) t
  val ( <*> ) : ('a -> 'b, 'err) t -> ('a, 'err) t -> ('b, 'err) t
end

module RESULT_MONAD : MONAD_FAIL with type ('ok, 'err) t = ('ok, 'err) Result.t

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
  val fold_bind_list : 'a list -> init:'b -> f:('a -> 'b -> ('b, 'c) t) -> ('b, 'c) t

  val fold_bind_map
    :  ('a, 'b, 'c) Base.Map.t
    -> init:'d
    -> f:('d -> 'a -> 'b -> ('d, 'e) t)
    -> ('d, 'e) t
end

module State : STATE_MONAD with type ('ok, 'err) t = int -> (int * 'ok, 'err) Result.t
