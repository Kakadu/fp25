type const =
  | IConst of int
  | FConst of float
  | BConst of bool
[@@deriving show]

type binop =
  | Add
  | Sub
  | Mul
  | Div
  | Eq
  | Neq
  | Leq
  | Geq
  | Lt
  | Gt
  | And
  | Or
  | AddF
  | SubF
  | MulF
  | DivF
[@@deriving show]

type wordtoken =
  | Keyword of string
  | Word of string
[@@deriving show]

type reclabel =
  | Recursive
  | Nonrecursive
[@@deriving show]

type expr =
  | EConst of const
  | EVar of string
  | EBinOp of binop * expr * expr
  | ELet of reclabel * letbind * expr
  | EIf of expr * expr * expr
  | EFun of expr * expr
  | EApp of expr * expr

and letbind = Bind of expr * expr [@@deriving show]

type toplevel =
  | TopLet of reclabel * letbind
  | TopExpr of expr

let pp_const fmt = function
  | IConst i -> Format.fprintf fmt "%d" i
  | FConst f -> Format.fprintf fmt "%g" f
  | BConst b -> Format.fprintf fmt "%b" b
;;

let pp_binop fmt = function
  | Add -> Format.fprintf fmt "+"
  | Sub -> Format.fprintf fmt "-"
  | Mul -> Format.fprintf fmt "*"
  | Div -> Format.fprintf fmt "/"
  | AddF -> Format.fprintf fmt "+."
  | SubF -> Format.fprintf fmt "-."
  | MulF -> Format.fprintf fmt "*."
  | DivF -> Format.fprintf fmt "/."
  | Eq -> Format.fprintf fmt "="
  | Neq -> Format.fprintf fmt "<>"
  | Leq -> Format.fprintf fmt "<="
  | Geq -> Format.fprintf fmt ">="
  | Lt -> Format.fprintf fmt "<"
  | Gt -> Format.fprintf fmt ">"
  | And -> Format.fprintf fmt "&&"
  | Or -> Format.fprintf fmt "||"
;;

let pp_reclabel fmt = function
  | Recursive -> Format.fprintf fmt "rec "
  | Nonrecursive -> ()
;;

let rec pp_expr fmt = function
  | EConst c -> pp_const fmt c
  | EVar v -> Format.fprintf fmt "%s" v
  | EBinOp (op, l, r) ->
    Format.fprintf fmt "@[<hov 2>(%a %a %a)@]" pp_expr l pp_binop op pp_expr r
  | EIf (c, t, e) ->
    Format.fprintf fmt "@[<v 2>if %a then@ %a@ else@ %a@]" pp_expr c pp_expr t pp_expr e
  | EFun (EVar x, body) -> Format.fprintf fmt "@[<hov 2>fun %s -> %a@]" x pp_expr body
  | EApp (f, arg) -> Format.fprintf fmt "@[<hov 2>(%a %a)@]" pp_expr f pp_expr arg
  | ELet (label, Bind (EVar x, e1), e2) ->
    Format.fprintf
      fmt
      "@[<v 2>let %a%s = %a in@ %a@]"
      pp_reclabel
      label
      x
      pp_expr
      e1
      pp_expr
      e2
  | _ -> Format.fprintf fmt "<unsupported>"
;;

let pp_letbind fmt = function
  | Bind (EVar x, e) -> Format.fprintf fmt "%s = %a" x pp_expr e
  | _ -> Format.fprintf fmt "<unsupported let binding>"
;;

let pp_toplevel fmt = function
  | TopExpr e -> Format.fprintf fmt "%a" pp_expr e
  | TopLet (label, bind) ->
    Format.fprintf fmt "let %a%a" pp_reclabel label pp_letbind bind
;;
