(** Copyright 2026, Dmitrii Kuznetsov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common

(** State + Error monad combinators *)
module STATEERROR : sig
  type ('st, 'a) t = 'st -> 'st * ('a, error) Result.t

  val return : 'a -> ('st, 'a) t
  val fail : error -> ('st, 'a) t
  val ( >>= ) : ('st, 'a) t -> ('a -> ('st, 'b) t) -> ('st, 'b) t
  val ( let* ) : ('st, 'a) t -> ('a -> ('st, 'b) t) -> ('st, 'b) t
  val ( *> ) : ('st, 'a) t -> ('st, 'b) t -> ('st, 'b) t
  val ( <|> ) : ('st, 'a) t -> ('st, 'a) t -> ('st, 'a) t
  val ( >>| ) : ('st, 'a) t -> ('a -> 'b) -> ('st, 'b) t
  val lift2 : ('a -> 'b -> 'c) -> ('st, 'a) t -> ('st, 'b) t -> ('st, 'c) t

  val lift3
    :  ('a -> 'b -> 'c -> 'd)
    -> ('st, 'a) t
    -> ('st, 'b) t
    -> ('st, 'c) t
    -> ('st, 'd) t

  val read : ('st, 'st) t
  val write : 'st -> ('st, unit) t
  val map : ('a -> ('st, 'b) t) -> 'a list -> ('st, 'b list) t
  val iter : ('a -> ('st, unit) t) -> 'a list -> ('st, unit) t
  val run : ('st, 'a) t -> 'st -> 'st * ('a, error) Result.t
end

(** Typechecker-specific monad operations *)
module TYPECHECK : sig
  open Ast

  type 'a t = (TypeCheck.state, 'a) STATEERROR.t

  val return : 'a -> 'a t
  val fail : error -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( *> ) : 'a t -> 'b t -> 'b t
  val ( <|> ) : 'a t -> 'a t -> 'a t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  val lift2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val lift3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
  val read_local : Common.obj_content IdMap.t t
  val read_local_el : ident -> (Common.obj_content option -> 'a t) -> 'a t
  val read_local_el_opt : ident -> Common.obj_content option t
  val read_local_el : ident -> Common.obj_content t
  val write_local : Common.obj_content IdMap.t -> unit t
  val write_local_el : ident -> Common.obj_content -> unit t
  val write_meth_type_opt : _type option -> unit t
  val write_meth_type : _type -> unit t
  val read_global : Common.context IdMap.t t
  val read_global_el : ident -> (Common.context option -> 'a t) -> 'a t
  val read_global_el_opt : ident -> Common.context option t
  val read_global_el : ident -> Common.context t
  val read_meth_type : _type option t
  val read_main_class : ident option t
  val write_main_class : ident option -> unit t
  val write_global : Common.context IdMap.t -> unit t
  val write_global_el : ident -> Common.context -> unit t
  val get_curr_class_name : ident t
  val write_curr_class_name : ident -> unit t
  val map : ('a -> 'b t) -> 'a list -> 'b list t
  val iter : ('a -> unit t) -> 'a list -> unit t
  val run : 'a t -> TypeCheck.state -> TypeCheck.state * ('a, error) Result.t
end
