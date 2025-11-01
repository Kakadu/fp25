val parse_structure : string -> (Ast.structure, string) Result.t
val parse_expression : string -> (Ast.expression, string) Result.t

(* testing stuff *)
val parse_pattern : string -> (Ast.pattern, string) Result.t
val parse_core_type : string -> (Ast.core_type, string) Result.t
