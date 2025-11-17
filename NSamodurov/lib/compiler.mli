type instr =
  | Access of int
  | Cur of instr list
  | Const of int
  | EndLet
  | Return
  | Grab
  | Apply
  | Let
  | Push
  | PushMark
  | AppTerm
  | Dummy
  | Update

val compile : Ast.brujin Ast.t -> instr list
val pp_instr : Format.formatter -> instr -> unit
