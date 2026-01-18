[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type rec_flag =
  | Recursive
  | NonRecursive

type 'a list1 = 'a * 'a list

type constant =
  | CUnit
  | CInt of int
  | CBool of bool

and pattern =
  | PAny
  | PConstant of constant
  | PVar of string
  | PTuple of pattern * pattern * pattern list
  | PConstruct of string * pattern option

type expression =
  | EConstant of constant
  | EVar of string
  | ETuple of expression * expression * expression list
  | EBinop of binop * expression * expression
  | ELet of rec_flag * (pattern * expression) list1 * expression
  | EFun of pattern * expression
  | EIf of expression * expression * expression
  | EApp of expression * expression
  | EConstruct of string * expression option
  | EMatch of expression * (pattern * expression) list1

and binop =
  | Add
  | Mul
  | Sub
  | Div
  | Eq
  | Ne
  | Le
  | Ge
  | Lt
  | Gt

type value_binding = pattern * expression

type type_declaration =
  { pty_params : string list (** ['a] is param in [type 'a list = ...] *)
  ; pty_name : string (** [list] is name in [type 'a list = ...] *)
  ; pty_kind : type_kind
  }

and type_kind =
  | Pty_abstract of core_type option (** [ type t ], [ type t = x ] *)
  | Pty_variants of (string * core_type option) list1
  (** [ type t = Some of int | None ] *)

and core_type =
  | Pty_var of string (** [ 'a, 'b ] are type variables in [ type ('a, 'b) ty = ... ] *)
  | Pty_arrow of core_type * core_type (** ['a -> 'b] *)
  | Pty_tuple of core_type * core_type * core_type list (** [ 'a * 'b * 'c ] *)
  | Pty_constr of string * core_type list (** [ int ], ['a option], [ ('a, 'b) list ] *)

type structure_item =
  | Pstr_value of rec_flag * value_binding list1 (** [ let x = ... ] *)
  | Pstr_type of type_declaration list1 (** [ type x = ... ] *)

type structure = structure_item list1

open Base.Format
open Base
open Pprint
open Pprint.Parens

let show_rec_flag = function
  | Recursive -> " rec "
  | NonRecursive -> " "
;;

let show_binop = function
  | Add -> "+"
  | Mul -> "*"
  | Sub -> "-"
  | Div -> "/"
  | Eq -> "=="
  | Ne -> "<>"
  | Le -> "<="
  | Ge -> ">="
  | Lt -> "<"
  | Gt -> ">"
;;

let pp_binop ppf x = fprintf ppf "%s" (show_binop x)

let show_constant = function
  | CUnit -> "()"
  | CInt x -> sprintf "%d" x
  | CBool x -> sprintf "%b" x
;;

let rec show_pattern ?(ctx = Parens.Free) = function
  | PAny -> "_"
  | PConstant c -> show_constant c
  | PVar name -> name
  | PTuple (a, b, xs) ->
    set_parens ~ctx [ Tuple; App; LeftSideFun; LeftSideLet; Binop ]
    @@ show_tuple (show_pattern ~ctx:Tuple) (a, b, xs)
  | PConstruct ("::", Some (PTuple (head, tail, []))) ->
    let rec helper acc = function
      | PConstruct ("::", Some (PTuple (hd, tl, []))) -> helper (hd :: acc) tl
      | PConstruct ("[]", None) -> show_list_brackets show_pattern (head :: List.rev acc)
      | _ as exp -> show_list_cons ~ctx show_pattern (head :: List.rev (exp :: acc))
    in
    helper [] tail
  | PConstruct (name, None) -> name
  | PConstruct (name, Some arg) ->
    set_parens ~ctx [ App; LeftSideFun; LeftSideLet ]
    @@ sprintf "%s %s" name (show_pattern ~ctx:App arg)
;;

let pp_constant ppf constant = fprintf ppf "%s" (show_constant constant)

let rec show_expression ?(ctx = Parens.Free) = function
  | EConstant x -> show_constant x
  | EVar x -> x
  | EBinop (binop, left, right) ->
    set_parens ~ctx [ App; Binop ]
    @@ sprintf
         "@[%s %s %s@]"
         (show_expression ~ctx:Binop left)
         (show_binop binop)
         (show_expression ~ctx:Binop right)
  | ETuple (a, b, xs) ->
    set_parens ~ctx [ Tuple; App; Binop ]
    @@ show_tuple (show_expression ~ctx:Tuple) (a, b, xs)
  | EConstruct ("::", Some (ETuple (head, tail, []))) ->
    let rec helper acc = function
      | EConstruct ("::", Some (ETuple (hd, tl, []))) -> helper (hd :: acc) tl
      | EConstruct ("[]", None) ->
        show_list_brackets show_expression (head :: List.rev acc)
      | _ as exp -> show_list_cons ~ctx show_expression (head :: List.rev (exp :: acc))
    in
    helper [] tail
  | EConstruct (name, None) -> name
  | EConstruct (name, Some arg) ->
    set_parens ~ctx [ App ] @@ sprintf "@[%s %s@]" name (show_expression ~ctx:App arg)
  | ELet (rec_flag, (vb, vbs), body) ->
    set_parens ~ctx [ App; Binop; Tuple ]
    @@ sprintf
         "@[%s in@;<1 2>%s@]"
         (show_value_binding rec_flag vb vbs)
         (show_expression ~ctx:Free body)
  | EApp (f, x) ->
    set_parens ~ctx [ App ]
    @@ sprintf "%s %s" (show_expression ~ctx:App f) (show_expression ~ctx:App x)
  | EFun (p, e) ->
    set_parens ~ctx [ Tuple; Binop; App ]
    @@ sprintf
         "@[fun %s ->@;<1 2>%s@]"
         (show_pattern ~ctx:LeftSideFun p)
         (show_expression ~ctx:RightSideFun e)
  | EIf (i, t, e) ->
    set_parens ~ctx [ Ite; Tuple; Binop; App ]
    @@ sprintf
         "@[if %s then %s else %s@]"
         (show_expression ~ctx:Ite i)
         (show_expression ~ctx:Ite t)
         (show_expression ~ctx:Ite e)
  | EMatch (e, (case1, cases)) ->
    let pp_case fmt (p, e) =
      Format.fprintf
        fmt
        "@[| %s -> %s@]"
        (show_pattern ~ctx:LeftSideMatch p)
        (show_expression ~ctx:RightSideMatch e)
    in
    set_parens ~ctx [ MatchWith; Tuple; Binop; App; RightSideMatch ]
    @@ Format.asprintf
         "@[match %s with@]@ %a"
         (show_expression ~ctx:MatchWith e)
         (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@;") pp_case)
         (case1 :: cases)

and show_value_binding rec_flag vb vbs =
  let pp_binding (p, e) =
    Format.asprintf
      "%s =@;<1 2>%s"
      (show_pattern ~ctx:LeftSideLet p)
      (show_expression ~ctx:RightSideLet e)
  in
  Format.asprintf
    "let%s%a"
    (show_rec_flag rec_flag)
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@;and ")
       (fun fmt b -> Format.fprintf fmt "%s" (pp_binding b)))
    (vb :: vbs)
;;

let rec show_core_type ?(ctx = Parens.Free) = function
  | Pty_var name -> name
  | Pty_arrow (a, b) ->
    set_parens ~ctx [ LeftSideArrow; App; Tuple ]
    @@ sprintf
         "%s -> %s"
         (show_core_type ~ctx:LeftSideArrow a)
         (show_core_type ~ctx:RightSideArrow b)
  | Pty_tuple (ct1, ct2, cts) ->
    set_parens ~ctx [ App; Tuple ]
    @@ sprintf "%s"
    @@ show_many ~sep:" * " (show_core_type ~ctx:Tuple) (ct1 :: ct2 :: cts)
  | Pty_constr (name, []) -> name
  | Pty_constr (name, [ param ]) ->
    set_parens ~ctx [ App ] @@ sprintf "%s %s" (show_core_type ~ctx:App param) name
  | Pty_constr (name, ct1 :: ct2 :: cts) ->
    set_parens ~ctx [ App ]
    @@ sprintf "(%s) %s" (show_tuple (show_core_type ~ctx:Free) (ct1, ct2, cts)) name
;;

let pp_core_type ppf core_type = fprintf ppf "@[%s@]" (show_core_type core_type)

let show_type_declaration td tds =
  let show_type_params td =
    match td.pty_params with
    | [] -> " "
    | [ x ] -> sprintf " %s " x
    | x :: xs -> sprintf " (%s) " (String.concat ~sep:", " (x :: xs))
  in
  let show_type_kind td =
    match td.pty_kind with
    | Pty_abstract None -> "@ "
    | Pty_abstract (Some ct) -> sprintf " = %s@ " (show_core_type ct)
    | Pty_variants (case, cases) ->
      let show_case = function
        | name, None -> sprintf "| %s@ " name
        | name, Some ct -> sprintf "| %s of %s@ " name (show_core_type ct)
      in
      sprintf " =@ %s@ " (String.concat (List.map ~f:show_case (case :: cases)))
  in
  let helper ~keyword td =
    Format.sprintf
      "%s%s%s%s"
      keyword
      (show_type_params td)
      td.pty_name
      (show_type_kind td)
  in
  sprintf "%s%s" (helper ~keyword:"type" td)
  @@ String.concat (List.map ~f:(helper ~keyword:"and") tds)
;;

let pp_type_declaration ppf td tds = fprintf ppf "%s" (show_type_declaration td tds)

let show_structure (item, items) =
  let helper = function
    | Pstr_value (rec_flag, (vb, vbs)) -> show_value_binding rec_flag vb vbs
    | Pstr_type (td, tds) -> show_type_declaration td tds
  in
  String.concat (List.map ~f:helper (item :: items))
;;

let show_expression = show_expression ~ctx:Free
let show_pattern = show_pattern ~ctx:Free
let show_core_type = show_core_type ~ctx:Free
let pp_structure ppf items = fprintf ppf "%s" (show_structure items)
let pp_pattern ppf patt = fprintf ppf "%s" (show_pattern patt)
let pp_expression ppf expr = fprintf ppf "@[%s@]" (show_expression expr)
