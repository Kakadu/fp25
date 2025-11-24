type typ =
  | TInt
  | TFun of typ * typ
  | TUnit
  | TVar of string

type type_env = (string, typ, Base.String.comparator_witness) Base.Map.t

type type_error =
  | UnboundVariable of string
  | TypeMismatch of typ * typ
  | OccursCheckError
  | InvalidCondition
  | ApplicationError
  | NotExpression

val show_type_error : type_error -> string
val typecheck_program : Ast.expr -> (typ, type_error) result
val print_typ : typ -> string
