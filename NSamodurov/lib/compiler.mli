type op =
  | Add
  | Sub
  | Mul
  | Div
  | Less
  | Great
  | LessEq
  | GreatEq
  | Equal
  | NeqPhysical
  | NeqStruct
  | And
  | Or

type instr =
  | Access of int (** Access variable *)
  | Cur of instr list (** Non-tail recursive closure *)
  | Const of int (** Put a constant to accumulator *)
  | Primitive of op (** Primitive operators *)
  | BranchIf of int (** Jump to specific offset if acc != 0 *)
  | Branch of int (** Unconditional jump*)
  | EndLet (** End of let "frame" *)
  | Return (** Return value and get to previous enviroment *)
  | Grab (** Get argument from argument stack *)
  | Apply (** Apply argument from accumulator *)
  | Let (** Start let "frame" *)
  | Push (** Push argument onto argument stack *)
  | PushMark (** Same as Push instruction but used first*)
  | AppTerm (** Tail-recursive version of EndLet *)
  | Dummy (** Push dummy symbol to enviroment *)
  | Update (** Substitute variable in enviroment *)

val compile : Ast.brujin Ast.t -> instr list
val pp_instr : Format.formatter -> instr -> unit
val list_of_apps : 'a Ast.t -> 'a Ast.t list
