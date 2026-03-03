(** Copyright 2025, Tenyaeva Ekaterina *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module StateR (S : sig
    type state
    type error
  end) : sig
  type state = S.state
  type error = S.error
  type 'a t = state -> state * ('a, error) result

  val return : 'a -> 'b -> 'b * ('a, 'c) result

  val ( >>= )
    :  ('a -> 'b * ('c, 'd) result)
    -> ('c -> 'b -> 'b * ('e, 'd) result)
    -> 'a
    -> 'b * ('e, 'd) result

  val ( >>| ) : ('a -> 'b * ('c, 'd) result) -> ('c -> 'e) -> 'a -> 'b * ('e, 'd) result

  val ( let* )
    :  ('a -> 'b * ('c, 'd) result)
    -> ('c -> 'b -> 'b * ('e, 'd) result)
    -> 'a
    -> 'b * ('e, 'd) result

  val ( let+ ) : ('a -> 'b * ('c, 'd) result) -> ('c -> 'e) -> 'a -> 'b * ('e, 'd) result
  val get : 'a -> 'a * ('a, 'b) result
  val put : 'a -> 'b -> 'a * (unit, 'c) result
  val fail : 'a -> 'b -> 'b * ('c, 'a) result
  val run : 'a -> 'a
end
