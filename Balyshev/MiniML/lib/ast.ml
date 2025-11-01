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

type type_declaration =
  { typedef_params : string list (** ['a] is param in [type 'a list = ...]  *)
  ; typedef_name : string (** [list] is name in [type 'a list = ...]  *)
  ; typedef_kind : type_kind
  }

and type_kind =
  | KAbstract of core_type option (** [ type t ], [ type t = x ] *)
  | KVariants of (string * core_type option) list1 (** [ type t = Some of int | None ]  *)

and core_type =
  | CTVar of string (** [ 'a, 'b ] are type variables in [ type ('a, 'b) ty = ... ] *)
  | CTArrow of core_type * core_type (** ['a -> 'b] *)
  | CTTuple of core_type * core_type * core_type list (** [ 'a * 'b * 'c ] *)
  | CTConstr of string * core_type list (** [ int ], ['a option], [ ('a, 'b) list ] *)

type structure_item =
  | SValue of value_binding (** [ let x = ... ] *)
  | SType of type_declaration list1 (** [ type x = ... ] *)

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
    sprintf "%s in %s" (show_vbs rec_flag vb vbs) (show_expression body)
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

and show_vbs rec_flag vb vbs =
  let show_vb p e = sprintf "%s = %s" (show_pattern p) (show_expression e) in
  sprintf
    "let%s%s"
    (if rec_flag == Recursive then " rec " else " ")
    (Base.String.concat
       ~sep:" and "
       (List.map (vb :: vbs) ~f:(fun (p, e) -> show_vb p e)))
;;

let pp_expression ppf expr = Format.fprintf ppf "@[%s@]" (show_expression expr)

let rec pp_core_type ppf = function
  | CTVar name -> fprintf ppf "%s" name
  | CTArrow (ctl, ctr) -> fprintf ppf "(%a -> %a)" pp_core_type ctl pp_core_type ctr
  | CTTuple (ct1, ct2, cts) ->
    fprintf ppf "@[(%a * %a" pp_core_type ct1 pp_core_type ct2;
    List.iter cts ~f:(fprintf ppf " * %a" pp_core_type);
    fprintf ppf ")@]"
  | CTConstr (name, []) -> fprintf ppf "@[%s@]" name
  | CTConstr (name, [ arg ]) -> fprintf ppf "@[(%a) %s@]" pp_core_type arg name
  | CTConstr (name, arg :: args) ->
    fprintf ppf "@[(%a" pp_core_type arg;
    List.iter args ~f:(fprintf ppf ", %a" pp_core_type);
    fprintf ppf ") %s@]" name
;;

let pp_type_params ppf td =
  match td.typedef_params with
  | [] -> fprintf ppf " "
  | [ x ] -> fprintf ppf " %s " x
  | x :: xs ->
    fprintf ppf "@[ (%s" x;
    List.iter xs ~f:(fprintf ppf ", %s");
    fprintf ppf ") @]"
;;

let pp_type_kind ppf td =
  match td.typedef_kind with
  | KAbstract None -> ()
  | KAbstract (Some ct) -> fprintf ppf "@[%a@]" pp_core_type ct
  | KVariants (case, cases) ->
    let pp_case ppf = function
      | name, None -> fprintf ppf "| %s" name
      | name, Some ct -> fprintf ppf "| %s of %a" name pp_core_type ct
    in
    fprintf ppf "@[<v>";
    List.iter (case :: cases) ~f:(fprintf ppf "@[%a@]@ " pp_case);
    fprintf ppf "@]"
;;

let pp_type_definition ppf (td, tds) =
  fprintf ppf "@[<v>";
  fprintf
    ppf
    "@[<v 2>@[type%a%s =@]@ @[%a@]@]@ "
    pp_type_params
    td
    td.typedef_name
    pp_type_kind
    td;
  List.iter tds ~f:(fun td ->
    fprintf
      ppf
      "@[<v 2>@[and%a%s =@]@ @[%a@]@]@ "
      pp_type_params
      td
      td.typedef_name
      pp_type_kind
      td);
  fprintf ppf "@]"
;;

(* TODO *)
let show_structure (item, items) =
  let helper = function
    | SValue (rec_flag, (vb, vbs)) -> show_vbs rec_flag vb vbs
    | SType (_, _) -> failwith "not implemented: show type declaration"
  in
  String.concat (List.map ~f:helper (item :: items))
;;

let pp_structure ppf (item, items) =
  let helper = function
    | SValue (rec_flag, (vb, vbs)) -> fprintf ppf "%s" (show_vbs rec_flag vb vbs)
    | SType tds -> fprintf ppf "%a" pp_type_definition tds
  in
  List.iter (item :: items) ~f:helper
;;

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
