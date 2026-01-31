{
    open Parser
}

rule token = parse
    | [' ' '\t' '\n'] { token lexbuf }
    | "let" { LET }
    | "rec" { REC }
    | "exception" { EXCEPTION }
    | "try" { TRY }
    | "with" { WITH }
    | "in" { IN }
    | "if" { IF }
    | "then" { THEN }
    | "else" { ELSE }
    | "()" { UNIT }
    | "fun" { FUN }
    | "->" { MAPSTO }
    | "true" { TRUE }
    | "false" { FALSE }
    | "not" { NOT }
    | "," { COMMA }
    | "inr" { INR }
    | "inl" { INL }
    | "match" { MATCH }
    | "println_int" as op { VAR op }
    | ['0'-'9']+ as num { INT (int_of_string num) }
    | ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']* as name { VAR name }
    | ['+' '*' '<' '>' '-'] as op { INFIXOP (String.make 1 op) }
    | "==" as op { INFIXOP op }
    | "||" as op { INFIXOP op }
    | "&&" as op { INFIXOP op }
    | "(" { LEFT_BRACK }
    | ")" { RIGHT_BRACK }
    | "=" { EQ }
    | "|" { CASE }
    | _ as c { raise (Failure (Printf.sprintf "Unknown character %c at position %d" c (Lexing.lexeme_start lexbuf))) }
    | eof { EOF }
