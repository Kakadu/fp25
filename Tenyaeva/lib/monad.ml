(** Copyright 2025, Tenyaeva Ekaterina *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module StateR (S : sig
    type state
    type error
  end) =
struct
  type state = S.state
  type error = S.error
  type 'a t = state -> state * ('a, error) result

  let return v st = st, Ok v

  let ( >>= ) m f st =
    let s1, r = m st in
    match r with
    | Error e -> s1, Error e
    | Ok v -> f v s1
  ;;

  let ( >>| ) m f = m >>= fun x -> return (f x)
  let ( let* ) = ( >>= )
  let ( let+ ) = ( >>| )
  let get st = st, Ok st
  let put new_st _st = new_st, Ok ()
  let fail e st = st, Error e
  let run st = st
end
