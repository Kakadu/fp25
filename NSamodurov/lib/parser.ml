(** Copyright 2021-2023, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast
module SMap = Map.Make (String)

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

type error = [ `Parsing_error of string ]

let pp_error ppf = function
  | `Parsing_error s -> Format.fprintf ppf "%s" s
;;

let parens = fun p -> char '(' *> p <* char ')'
let to_left_assoc s h tl = List.fold_left (fun acc x -> EBop (s, acc, x)) h tl
let multi_sum h tl = to_left_assoc Plus h tl
let multi_prod h tl = to_left_assoc Asterisk h tl
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
    [| [ (ws *> char '+' <* ws, fun a b -> EBop (Plus, a, b))
       ; (ws *> char '-' <* ws, fun a b -> EBop (Minus, a, b))
       ]
     ; [ (ws *> char '*' <* ws, fun a b -> EBop (Asterisk, a, b))
       ; (ws *> char '/' <* ws, fun a b -> EBop (Slash, a, b))
       ]
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

let to_brujin expr =
  let open Monads.Env in
  let open Monads.Env.Syntax in
  let rec helper =
    fun bound -> function
      | EConst (Int x) -> return (int x)
      | ELet _ -> failwith "unimpl"
      | EBop (s, a, b) ->
        let* a = helper bound a in
        let* b = helper bound b in
        return (EBop (s, a, b))
      | EAbs (x, e) ->
        let* map = read in
        if SMap.mem x map
        then
          let* e = helper (x :: bound) e in
          return (EAbs (Blank, e))
        else (
          let i = SMap.cardinal map in
          let* () = write (SMap.add x i map) in
          let* e = helper bound e in
          return (EAbs (Blank, e)))
      | EApp (e1, e2) ->
        let* b1 = helper bound e1 in
        let* b2 = helper bound e2 in
        return (EApp (b1, b2))
      | EVar v ->
        let* map = read in
        (match List.find_index (String.equal v) bound with
         | None ->
           if SMap.mem v map
           then return (EVar (Index (SMap.find v map + List.length bound)))
           else (
             let i = SMap.cardinal map in
             let* () = write (SMap.add v i map) in
             return (EVar (Index (i + List.length bound))))
         | Some i -> return (EVar (Index i)))
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
