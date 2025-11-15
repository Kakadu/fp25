(** Copyright 2021-2023, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast
open Monads

let ws =
  skip_while (function
    | ' ' | '\t' | '\n' | '\r' -> true
    | _ -> false)
;;

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

let int_of_char_list = fun x -> string_of_char_list x |> int_of_string

let varname =
  alpha
  >>= fun h ->
  many (alpha <|> digit <|> char '_') >>= fun tl -> return @@ string_of_char_list (h :: tl)
;;

let sign_of_char = function
  | '+' -> Plus
  | '-' -> Minus
  | '*' -> Asterisk
  | '/' -> Slash
  | c -> Ast.Other c
;;

let number =
  digit >>= fun h -> many digit >>= fun tl -> return @@ int_of_char_list (h :: tl)
;;

let conde = function
  | [] -> fail "empty conde"
  | h :: tl -> List.fold_left ( <|> ) h tl
;;

type dispatch =
  { apps : dispatch -> string Ast.t Angstrom.t
  ; single : dispatch -> string Ast.t Angstrom.t
  }

let parens = fun p -> char '(' *> p <* char ')'

(* let bop s a b = Abs                         *)
let to_left_assoc s h tl = List.fold_left (fun acc x -> s acc x) h tl
let multi_sum h tl = to_left_assoc add h tl
let multi_prod h tl = to_left_assoc sub h tl
let int c = EConst (Int c)

let prio expr table =
  let length = Array.length table in
  let rec helper level =
    if level >= length
    then expr
    else (
      let xs = table.(level) in
      return (List.fold_left (fun acc (op, r) -> op acc r))
      <*> helper (level + 1)
      <*> many
          @@ conde
          @@ List.map
               (fun (op, f) -> op *> helper (level + 1) >>= fun r -> return (f, r))
               xs)
  in
  helper 0
;;

let expr =
  let small_expr = number >>| int in
  prio
    small_expr
    [| [ ws *> char '+' <* ws, add; ws *> char '-' <* ws, sub ]
     ; [ ws *> char '*' <* ws, mul; ws *> char '/' <* ws, div ]
    |]
;;

let parse_lam =
  let single pack =
    fix (fun _ ->
      conde
        [ parens (pack.apps pack) <?> "Parentheses expected"
        ; (string "let" *> ws *> varname
           >>= fun v ->
           ws *> pack.apps pack
           >>= fun e1 ->
           ws *> string "in" *> ws *> pack.apps pack
           >>= fun e2 -> return (Ast.ELet (Ast.NotRecursive, v, e1, e2)))
          (* Not recursive be default is temp *)
        ; (string "fun" *> many1 (ws *> varname)
           <* ws
           <* string "->" *> return ()
           >>= fun list ->
           pack.apps pack
           >>= fun b -> return (List.fold_right (fun x acc -> Ast.EAbs (x, acc)) list b))
        ; (varname <* ws >>= fun v -> return (Ast.EVar v))
        ; (expr <* ws >>= fun ast -> return ast)
        ])
  in
  let apps pack =
    many1 (ws *> pack.single pack <* ws)
    >>= function
    | [] -> fail "bad syntax"
    | x :: xs -> return @@ List.fold_left (fun l r -> Ast.EApp (l, r)) x xs
  in
  { single; apps }
;;

(** Monad for de brujin global context  *)
module Env : STATE_MONAD = struct
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

module Context = struct
  include Map.Make (String)

  let extend k v m = add k (v + reserved) m
end

let to_brujin expr =
  let open Env in
  let open Env.Syntax in
  let rec helper =
    fun bound -> function
      | EVar "+" -> return plus
      | EVar "-" -> return minus
      | EVar "*" -> return asterisk
      | EVar "/" -> return slash
      | EVar v ->
        let* map = read in
        (match List.find_index (String.equal v) bound with
         | None ->
           if Context.mem v map
           then return (evar (Index (Context.find v map + List.length bound)))
           else (
             let i = Context.cardinal map in
             let* () = write (Context.extend v i map) in
             return (evar (Index (i + List.length bound + reserved))))
         | Some i -> return (EVar (Index (i + reserved))))
      | EConst (Int x) -> return (int x)
      | ELet _ -> failwith "unimpl"
      | EAbs (x, e) ->
        let* map = read in
        if Context.mem x map
        then (
          let bound = x :: bound in
          let* e = helper bound e in
          return (eabs (Index (List.length bound)) e))
        else (
          let i = Context.cardinal map in
          let* () = write (Context.extend x i map) in
          let bound = x :: bound in
          let* e = helper bound e in
          return (eabs (Index (List.length bound)) e))
      | EApp (e1, e2) ->
        let* b1 = helper bound e1 in
        let* b2 = helper bound e2 in
        return (eapp b1 b2)
  in
  run (helper [] expr) Context.empty
;;

let parse str =
  match
    Angstrom.parse_string (parse_lam.apps parse_lam) ~consume:Angstrom.Consume.All str
  with
  | Result.Ok x -> Result.Ok x
  | Error er -> Result.Error (`Parsing_error er)
;;
