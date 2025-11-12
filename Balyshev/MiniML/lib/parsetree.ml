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
  | Cons
  | Eq
  | Ne
  | Le
  | Ge
  | Lt
  | Gt

type value_binding = pattern * expression

type type_declaration =
  { pty_params : string list (** ['a] is param in [type 'a list = ...]  *)
  ; pty_name : string (** [list] is name in [type 'a list = ...]  *)
  ; pty_kind : type_kind
  }

and type_kind =
  | Pty_abstract of core_type option (** [ type t ], [ type t = x ] *)
  | Pty_variants of (string * core_type option) list1
  (** [ type t = Some of int | None ]  *)

and core_type =
  | Pty_var of string (** [ 'a, 'b ] are type variables in [ type ('a, 'b) ty = ... ] *)
  | Pty_arrow of core_type * core_type (** ['a -> 'b] *)
  | Pty_tuple of core_type * core_type * core_type list (** [ 'a * 'b * 'c ] *)
  | Pty_constr of string * core_type list (** [ int ], ['a option], [ ('a, 'b) list ] *)

type structure_item =
  | Pstr_value of rec_flag * value_binding list1 (** [ let x = ... ] *)
  | Pstr_type of type_declaration list1 (** [ type x = ... ] *)

type structure = structure_item list1

let string_of_binop = function
  | Add -> "+"
  | Mul -> "*"
  | Sub -> "-"
  | Div -> "/"
  | Cons -> "::"
  | Eq -> "="
  | Ne -> "<>"
  | Le -> "<="
  | Ge -> ">="
  | Lt -> "<"
  | Gt -> ">"
;;

open Format

let show_tuple ?(sep = ", ") show_item (a, b, xs) =
  String.concat sep (List.map (fun x -> show_item x) (a :: b :: xs)) |> sprintf "@[(%s)@]"
;;

open Base

let rec show_pattern = function
  | PAny -> sprintf "_"
  | PVar name -> sprintf "%s" name
  | PTuple (a, b, xs) -> show_tuple show_pattern (a, b, xs)
  | PConstruct ("[]", None) -> "[]"
  | PConstruct ("::", Some (PTuple (head, tail, []))) ->
    let rec helper acc = function
      | PConstruct ("::", Some (PTuple (hd, tl, []))) -> helper (hd :: acc) tl
      | PConstruct ("[]", None) ->
        sprintf
          "@[[ %s ]@]"
          (String.concat ~sep:"; " (List.map ~f:show_pattern (head :: List.rev acc)))
      | _ as exp ->
        String.concat
          ~sep:" :: "
          (List.map ~f:show_pattern (head :: List.rev (exp :: acc)))
    in
    helper [] tail
  | PConstruct (name, None) -> sprintf "%s" name
  | PConstruct (name, Some arg) -> sprintf "@[%s (%s)@]" name (show_pattern arg)
;;

let pp_pattern ppf patt = Format.fprintf ppf "%s" (show_pattern patt)

let show_constant = function
  | CUnit -> sprintf "()"
  | CInt x -> sprintf "%d" x
  | CBool x -> sprintf "%b" x
;;

let pp_constant ppf constant = Format.fprintf ppf "%s" (show_constant constant)

let rec show_expression = function
  | EConstant x -> show_constant x
  | EVar x -> sprintf "%s" x
  | EBinop (binop, left, right) ->
    sprintf
      "@[(%s %s %s)@]"
      (show_expression left)
      (string_of_binop binop)
      (show_expression right)
  | ETuple (a, b, xs) -> show_tuple show_expression (a, b, xs)
  | EConstruct ("::", Some (ETuple (head, tail, []))) ->
    let rec helper acc = function
      | EConstruct ("::", Some (ETuple (hd, tl, []))) -> helper (hd :: acc) tl
      | EConstruct ("[]", None) ->
        sprintf
          "@[[ %s ]@]"
          (String.concat ~sep:"; " (List.map ~f:show_expression (head :: List.rev acc)))
      | _ as exp ->
        String.concat
          ~sep:" :: "
          (List.map ~f:show_expression (head :: List.rev (exp :: acc)))
    in
    helper [] tail
  | EConstruct ("[]", None) -> sprintf "[]"
  | EConstruct (name, None) -> sprintf "%s" name
  | EConstruct (name, Some arg) -> sprintf "@[%s (%s)@]" name (show_expression arg)
  | ELet (rec_flag, (vb, vbs), body) ->
    sprintf "%s in %s" (show_value_binding rec_flag vb vbs) (show_expression body)
  | EApp (f, x) -> sprintf "%s %s" (show_expression f) (show_expression x)
  | EFun (p, e) -> sprintf "fun %s -> %s" (show_pattern p) (show_expression e)
  | EIf (i, t, e) ->
    sprintf
      "if %s then %s else %s"
      (show_expression i)
      (show_expression t)
      (show_expression e)
  | EMatch (e, (pe, pes)) ->
    let show_case (p, e) = sprintf "| %s -> %s" (show_pattern p) (show_expression e) in
    sprintf
      "(match %s with@ %s)"
      (show_expression e)
      (Base.String.concat ~sep:(sprintf "@ ") (Base.List.map ~f:show_case (pe :: pes)))

and show_value_binding rec_flag vb vbs =
  let helper p e = sprintf "%s = %s" (show_pattern p) (show_expression e) in
  sprintf
    "let%s%s"
    (if rec_flag == Recursive then " rec " else " ")
    (String.concat ~sep:" and " (List.map (vb :: vbs) ~f:(fun (p, e) -> helper p e)))
;;

let pp_expression ppf expr = Format.fprintf ppf "@[%s@]" (show_expression expr)

let rec show_core_type = function
  | Pty_var name -> sprintf "%s" name
  | Pty_arrow (a, b) -> sprintf "(%s -> %s)" (show_core_type a) (show_core_type b)
  | Pty_tuple (ct1, ct2, cts) -> show_tuple ~sep:" * " show_core_type (ct1, ct2, cts)
  | Pty_constr (name, []) -> sprintf "%s" name
  | Pty_constr (name, args) ->
    sprintf "(%s) %s" (String.concat ~sep:", " (List.map ~f:show_core_type args)) name
;;

let pp_core_type ppf core_type = Format.fprintf ppf "@[%s@]" (show_core_type core_type)

let show_type_params td =
  match td.pty_params with
  | [] -> " "
  | [ x ] -> sprintf " %s " x
  | x :: xs -> sprintf " (%s) " (String.concat ~sep:", " (x :: xs))
;;

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
;;

let show_type_declaration td tds =
  let helper td =
    sprintf "%s%s%s" (show_type_params td) td.pty_name (show_type_kind td)
  in
  sprintf "type%s" (String.concat ~sep:"and" (List.map ~f:helper (td :: tds)))
;;

let pp_type_declaration ppf td tds = fprintf ppf "%s" (show_type_declaration td tds)

let show_structure (item, items) =
  let helper = function
    | Pstr_value (rec_flag, (vb, vbs)) -> show_value_binding rec_flag vb vbs
    | Pstr_type (td, tds) -> show_type_declaration td tds
  in
  String.concat (List.map ~f:helper (item :: items))
;;

let pp_structure ppf items = fprintf ppf "%s" (show_structure items)
