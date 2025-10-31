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

type value_binding = rec_flag * (pattern * expression) list1
type structure_item = SValue of value_binding
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

let show_tuple show_item (a, b, xs) =
  String.concat ", " (List.map (fun x -> show_item x) (a :: b :: xs))
  |> sprintf "@[(%s)@]"
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
    let show_vb p e = sprintf "%s = %s " (show_pattern p) (show_expression e) in
    sprintf
      "let%s%sin %s"
      (if rec_flag == Recursive then " rec " else " ")
      (Base.String.concat
         ~sep:"and "
         (List.map (vb :: vbs) ~f:(fun (p, e) -> show_vb p e)))
      (show_expression body)
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
;;

let pp_expression ppf expr = Format.fprintf ppf "@[%s@]" (show_expression expr)

type ty =
  | TUnit
  | TInt
  | TBool
  | TVar of string
  | TArrow of ty * ty
  | TProd of ty * ty * ty list

let rec show_ty = function
  | TUnit -> "unit"
  | TInt -> "int"
  | TBool -> "bool"
  | TVar name -> name
  | TArrow (a, b) -> sprintf "(%s -> %s)" (show_ty a) (show_ty b)
  | TProd (a, b, xs) -> show_tuple show_ty (a, b, xs)
;;

let pp_ty ppf ty = Format.fprintf ppf "%s" (show_ty ty)
