(** Copyright 2025, Tenyaeva Ekaterina *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module type STATER = sig
  type 'a t

  val return : 'a -> 'b -> 'b * ('a, 'c) result
  val fail : 'a -> 'b -> 'b * ('c, 'a) result

  val ( >>= )
    :  ('a -> 'b * ('c, 'd) result)
    -> ('c -> 'b -> 'b * ('e, 'd) result)
    -> 'a
    -> 'b * ('e, 'd) result

  val ( let* )
    :  ('a -> 'b * ('c, 'd) result)
    -> ('c -> 'b -> 'b * ('e, 'd) result)
    -> 'a
    -> 'b * ('e, 'd) result

  val ( >>| ) : ('a -> 'b * ('c, 'd) result) -> ('c -> 'e) -> 'a -> 'b * ('e, 'd) result
  val ( let+ ) : ('a -> 'b * ('c, 'd) result) -> ('c -> 'e) -> 'a -> 'b * ('e, 'd) result
  val get : 'a -> 'a * ('a, 'b) result
  val put : 'a -> 'b -> 'a * (unit, 'c) result
end

module StateR (_ : sig
    type state
    type error
  end) : STATER
