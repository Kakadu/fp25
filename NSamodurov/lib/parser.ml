(** Copyright 2021-2023, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast
module SMap = Map.Make (String)

module type StateMonad = sig
  type ('s, 'a) t

  val return : 'a -> ('s, 'a) t
  val bind : ('s, 'a) t -> ('a -> ('s, 'b) t) -> ('s, 'b) t

  module Syntax : sig
    val ( >>= ) : ('s, 'a) t -> ('a -> ('s, 'b) t) -> ('s, 'b) t
    val ( let* ) : ('s, 'a) t -> ('a -> ('s, 'b) t) -> ('s, 'b) t
  end

  val read : ('s, 's) t
  val run : ('s, 'a) t -> 's -> 'a
  val write : 's -> ('s, unit) t
end

(** Monad for global context  *)
module Env : StateMonad = struct
  type ('s, 'a) t = 's -> 's * 'a

  let return : 'a -> ('s, 'a) t = fun x s -> s, x

  let bind : ('s, 'a) t -> ('a -> ('s, 'b) t) -> ('s, 'b) t =
    fun m f st ->
    let st, x = m st in
    f x st
  ;;

  module Syntax = struct
    let ( let* ) = bind
    let ( >>= ) = bind
  end

  let read : ('s, 's) t = fun st -> st, st
  let run : ('s, 'a) t -> 's -> 'a = fun f st -> snd (f st)
  let write : 's -> ('s, unit) t = fun x _ -> x, ()
end

let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let spaces = skip_while is_space

let alpha =
  satisfy (function
    | 'a' .. 'z' -> true
    | 'A' .. 'Z' -> true
    | _ -> false)
;;

let digit =
  satisfy (function
    | '0' .. '9' -> true
    | _ -> false)
;;

let string_of_char_list =
  fun list ->
  let buf = Buffer.create 1024 in
  List.iter (Buffer.add_char buf) list;
  Buffer.contents buf
;;

let varname =
  alpha
  >>= fun h ->
  many (alpha <|> digit <|> char '_') >>= fun tl -> return @@ string_of_char_list (h :: tl)
;;

let conde = function
  | [] -> fail "empty conde"
  | h :: tl -> List.fold_left ( <|> ) h tl
;;

type dispatch =
  { apps : dispatch -> string Ast.t Angstrom.t
  ; single : dispatch -> string Ast.t Angstrom.t
  }

type error = [ `Parsing_error of string ]

let pp_error ppf = function
  | `Parsing_error s -> Format.fprintf ppf "%s" s
;;

let parse_lam =
  let single pack =
    fix (fun _ ->
      conde
        [ char '(' *> pack.apps pack <* char ')' <?> "Parentheses expected"
        ; (string "fun" *> many1 (spaces *> varname)
           <* spaces
           <* string "->" *> return ()
           >>= fun list ->
           pack.apps pack
           >>= fun b -> return (List.fold_right (fun x acc -> Ast.Abs (x, acc)) list b))
        ; (varname <* spaces >>= fun c -> return (Ast.Var c))
        ])
  in
  let apps pack =
    many1 (spaces *> pack.single pack <* spaces)
    >>= function
    | [] -> fail "bad syntax"
    | x :: xs -> return @@ List.fold_left (fun l r -> Ast.App (l, r)) x xs
  in
  { single; apps }
;;

let to_brujin (expr : string Ast.t) =
  let open Env in
  let open Env.Syntax in
  let rec helper =
    fun bound -> function
      | Integer x -> return (Integer x)
      | Abs (x, e) ->
        let* map = read in
        if SMap.mem x map
        then (
          let bound = x :: bound in
          let* e = helper bound e in
          return (Abs (Blank, e)))
        else (
          let i = SMap.cardinal map in
          let* () = write (SMap.add x i map) in
          let* e = helper bound e in
          return (Abs (Blank, e)))
      | App (e1, e2) ->
        let* b1 = helper bound e1 in
        let* b2 = helper bound e2 in
        return (App (b1, b2))
      | Var v ->
        let* map = read in
        (match List.find_index (String.equal v) bound with
         | None ->
           if SMap.mem v map
           then return (Var (Index (SMap.find v map + List.length bound)))
           else (
             let i = SMap.cardinal map in
             let* () = write (SMap.add v i map) in
             return (Var (Index (i + List.length bound))))
         | Some i -> return (Var (Index i)))
  in
  run (helper [] expr) SMap.empty
;;

let parse str =
  match
    Angstrom.parse_string (parse_lam.apps parse_lam) ~consume:Angstrom.Consume.All str
  with
  | Result.Ok x -> Result.Ok x
  | Error er -> Result.Error (`Parsing_error er)
;;
