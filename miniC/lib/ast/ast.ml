(** flag for definition of scope *)
type var_scope = 
  | LocalVar
  | GlobalVar
[@@deriving show { with_path = false }]

(** id for var *)
type ident = string [@@deriving show {  with_path = false}]

(** operations *)
type operation_id = 
  | OpAdd    (** element1 +  element2 *)
  | OpMul    (** element1 *  element2 *)
  | OpSub    (** element1 -  element1 *)
  | OpDiv    (** element1 /  element2 *)
  | OpMod    (** element1 %  element2 *)
  | OpEq     (** element1 == element2 *)
  | OpNeq    (** element1 != element2 *)
  | OpConcat (** element1 ^  element2 *)
  | OpLte    (** element1 <= element2 *)
  | OpLt     (** element1 <  element2 *)
  | OpOr     (** element1 || element2 *)
  | OpAnd    (** element1 && element2 *)
[@@deriving show { with_path = false}]

(** code code_block *)
type code_block = statement list

and statement = 
  | StatAssign of var_scope * ident * expression                 (** [LocalVar|GlobalVar] id = expression *)
  | StatIf of (expression * code_block) list * code_block option (** [if expression1 then ... else if ... else ...] *)
  | StatWhile of expression * code_block                         (** [while expression do ... end] *)
  | StatDo of code_block                                         (** [do ... end] ) *)
  | StatReturn of expression list                                (** [return expression1, expression2, ...] *)
  | StatCall of apply                                            (** [call expression] *)
  | StatBreak                                                    (** [break] *)
[@@deriving show { with_path = false} ]

and apply = Call of expression * expression list

and expression = 
  | ExpFalse                                               (** [false] *)
  | ExpTrue                                                (** [true] *)
  | ExpString of string                                    (** ["dsl"] *)
  | ExpNumber of float                                     (** [52] *)
  | ExpFunction of ident list * code_block                 (** [function (ident1, ident2, ...) ...]*)
  | ExpOperation of operation_id * expression * expression (** [element1, element2] *) 
  | ExpLhs of ident                                        (** name *)
  | ExpCall of apply                                       (** function call in expression *)
  | ExpNull                                                (** [null] *)                                                
[@@deriving show { with_path = false} ]