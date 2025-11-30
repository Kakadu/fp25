[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

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

let conde = function
  | [] -> fail "empty conde"
  | h :: tl -> List.fold_left ( <|> ) h tl
;;

let keyword = function
  | "let" | "in" | "if" | "then" | "else" | "->" | "fun" | "true" | "false" | "print" ->
    true
  | _ -> false
;;

let varname =
  let* str =
    let* h = alpha in
    let* tl = many (alpha <|> digit <|> char '_') in
    return @@ string_of_char_list (h :: tl)
  in
  if keyword str then fail "keyword" else return str
;;

let number =
  digit >>= fun h -> many digit >>= fun tl -> return @@ int_of_char_list (h :: tl)
;;

let bool_string =
  string "true"
  <|> string "false"
  >>= function
  | "true" -> return true
  | "false" -> return false
  | _ -> fail "nobool"
;;

type dispatch =
  { expr : dispatch -> string Ast.t Angstrom.t
  ; apps : dispatch -> string Ast.t Angstrom.t
  ; single : dispatch -> string Ast.t Angstrom.t
  }

let parens = fun p -> char '(' *> p <* char ')' <?> "Parentheses expected"
let var v = EVar v
let int c = EConst (Int c)
let bool c = EConst (Bool c)

let prio expr table =
  let length = Array.length table in
  let rec helper level =
    if level >= length
    then expr
    else (
      let xs = table.(level) in
      return (List.fold_left (fun acc (p, c) -> p acc c))
      <*> helper (level + 1)
      <*> many
          @@ conde
          @@ List.map
               (fun (p, c) -> p *> helper (level + 1) >>= fun r -> return (c, r))
               xs)
  in
  helper 0
;;

let parse_lam =
  let expr pack =
    prio
      (pack.apps pack)
      [| [ ws *> char '<' <* ws, le
         ; ws *> char '>' <* ws, gr
         ; ws *> char '<' <* char '=' <* ws, leq
         ; ws *> char '>' <* char '=' <* ws, grq
         ; ws *> char '=' <* ws, eq
         ; ws *> char '!' <* char '=' <* ws, neq_phy
         ; ws *> char '<' <* char '>' <* ws, neq_str
         ; ws *> char '&' <* char '&' <* ws, andl
         ; ws *> char '|' <* char '|' <* ws, orl
         ]
       ; [ ws *> char '+' <* ws, add; ws *> char '-' <* ws, sub ]
       ; [ ws *> char '*' <* ws, mul; ws *> char '/' <* ws, div ]
      |]
  in
  let single pack =
    fix (fun _ ->
      conde
        [ parens (pack.expr pack)
        ; varname <* ws >>| var
        ; number <* ws >>| int
        ; bool_string <* ws >>| bool
        ; (string "print" *> ws *> pack.expr pack
           >>= fun v -> return (Ast.EApp (Ast.EVar "print", v)))
        ; (string "if" *> ws *> pack.expr pack
           >>= fun p ->
           string "then" *> ws *> pack.expr pack
           >>= fun e1 ->
           string "else" *> ws *> pack.expr pack
           >>= fun e2 -> return (Ast.EIf (p, e1, e2)))
        ; (string "let rec" *> ws *> many1 (ws *> varname)
           <* ws
           <* string "="
           <* ws
           >>= fun v ->
           ws *> pack.expr pack
           >>= fun e1 ->
           ws *> string "in" *> ws *> pack.expr pack
           >>= fun e2 ->
           match v with
           | [] -> fail "Should not happend"
           | v :: [] -> return (Ast.ELet (Ast.Recursive, v, e1, e2))
           | v :: args ->
             let e1 = List.fold_right (fun x acc -> Ast.EAbs (x, acc)) args e1 in
             return (Ast.ELet (Ast.Recursive, v, e1, e2)))
        ; (string "let" *> ws *> many1 (ws *> varname)
           <* ws
           <* string "="
           <* ws
           >>= fun v ->
           ws *> pack.expr pack
           >>= fun e1 ->
           ws *> string "in" *> ws *> pack.expr pack
           >>= fun e2 ->
           match v with
           | [] -> fail "Should not happend"
           | v :: [] -> return (Ast.ELet (Ast.NotRecursive, v, e1, e2))
           | v :: args ->
             let e1 = List.fold_right (fun x acc -> Ast.EAbs (x, acc)) args e1 in
             return (Ast.ELet (Ast.NotRecursive, v, e1, e2)))
        ; (string "fun" *> ws *> many1 (varname <* ws)
           >>= fun list ->
           string "->" *> pack.expr pack
           >>= fun b -> return (List.fold_right (fun x acc -> Ast.EAbs (x, acc)) list b))
        ])
  in
  let apps pack =
    many1 (ws *> pack.single pack <* ws)
    >>= function
    | [] -> fail "bad syntax"
    | h :: tl -> return @@ List.fold_left (fun l r -> Ast.EApp (l, r)) h tl
  in
  { expr; apps; single }
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

  let extend = add
end

(* List function from newer ocaml stdlib *)
let find_index p =
  let rec helper i = function
    | [] -> None
    | a :: _ when p a -> Some i
    | _ :: l -> helper (i + 1) l
  in
  helper 0
;;

let to_brujin expr =
  let open Env in
  let open Env.Syntax in
  let rec helper =
    fun bound -> function
      | EVar "+" -> return @@ EVar (Index (-1))
      | EVar "-" -> return @@ EVar (Index (-2))
      | EVar "*" -> return @@ EVar (Index (-3))
      | EVar "/" -> return @@ EVar (Index (-4))
      | EVar "<" -> return @@ EVar (Index (-5))
      | EVar ">" -> return @@ EVar (Index (-6))
      | EVar "<=" -> return @@ EVar (Index (-7))
      | EVar ">=" -> return @@ EVar (Index (-8))
      | EVar "=" -> return @@ EVar (Index (-9))
      | EVar "!=" -> return @@ EVar (Index (-10))
      | EVar "<>" -> return @@ EVar (Index (-11))
      | EVar "&&" -> return @@ EVar (Index (-12))
      | EVar "||" -> return @@ EVar (Index (-13))
      | EVar "print" -> return @@ EVar (Index (-14))
      | EVar v ->
        let* map = read in
        (match find_index (String.equal v) bound with
         | None ->
           if Context.mem v map
           then return (evar (Index (Context.find v map + List.length bound)))
           else (
             let i = Context.cardinal map in
             let* () = write (Context.extend v i map) in
             return (evar (Index (i + List.length bound))))
         | Some i -> return (EVar (Index i)))
      | EConst (Int x) -> return (int x)
      | EConst (Bool x) -> return (bool x)
      | ELet (flag, v, e1, e2) ->
        let* e1 = helper bound e1 in
        let* e2 = helper (v :: bound) e2 in
        return (ELet (flag, Index (List.length bound), e1, e2))
      | EIf (pred, e1, e2) ->
        let* pred = helper bound pred in
        let* e1 = helper bound e1 in
        let* e2 = helper bound e2 in
        return (EIf (pred, e1, e2))
      | EAbs (x, e) ->
        let* e = helper (x :: bound) e in
        return (eabs (Index (List.length bound)) e)
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
  | Error er -> Result.Error (`ParsingError er)
;;
