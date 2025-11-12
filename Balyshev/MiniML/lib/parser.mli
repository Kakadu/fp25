val parse_structure : string -> (Parsetree.structure, string) Result.t
val parse_expression : string -> (Parsetree.expression, string) Result.t

(* testing stuff *)
val parse_pattern : string -> (Parsetree.pattern, string) Result.t
val parse_core_type : string -> (Parsetree.core_type, string) Result.t
