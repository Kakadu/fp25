(* Abstract Syntax Tree for miniML language *)

(** Constant values in the language *)
type constant =
  | CInt of int (** Integer literals *)
  | CUnit (** Unit value - returned by statements *)

(** Patterns for variable binding *)
type pattern = PVar of string (** Variable pattern - binds to a name *)

(** Recursion flag for let bindings *)
type rec_flag =
  | NonRec (** Non-recursive binding *)
  | Rec (** Recursive binding *)

(** Binary operators *)
type binop =
  | Plus (** Integer addition: + *)
  | Minus (** Integer subtraction: - *)
  | Asteriks (** Integer multiplication: * *)
  | Dash (** Integer division: / *)
  | Equals (** Integer equality: = *)
  | MoreThan (** Integer greater than: > *)
  | LessThan (** Integer less than: < *)
  | EqLess (** Integer less than or equal: <= *)
  | EqMore (** Integer greater than or equal: >= *)

(** Expressions in miniML language *)
type expr =
  | Constant of constant (** Constant values - integers and unit *)
  | Var of pattern (** Variable reference - looks up a bound name *)
  | Let of rec_flag * pattern * expr * expr option
  (** Let binding:
      - rec_flag: whether binding is recursive
      - pattern: variable name to bind
      - expr: value to bind
      - expr option: optional body expression (if None, it's a statement) *)
  | Fun of pattern * expr
  (** Lambda abstraction:
      - pattern: parameter name
      - expr: function body *)
  | App of expr * expr
  (** Function application:
      - expr: function expression
      - expr: argument expression *)
  | Binop of binop * expr * expr
  (** Binary operation:
      - binop: operator
      - expr: left operand
      - expr: right operand *)
  | Conditional of expr * expr * expr option
  (** Conditional expression:
      - expr: condition (must evaluate to int, 0=false, non-zero=true)
      - expr: then branch
      - expr option: optional else branch *)

(** Notes:
    - Evaluation strategy: Call-by-value (CBV)
    - All binary operators work only on integers
    - Conditionals use integers as booleans (0 = false, non-zero = true)
    - Functions are curried: fun x y -> ... desugars to fun x -> fun y -> ...
    - Let bindings without 'in' are statements, with 'in' are expressions
    - Type system supports integers, functions, and unit *)
