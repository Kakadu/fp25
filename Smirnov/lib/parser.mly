%{
    open Ast
%}

%token <string> BASETYPE
%token LET
%token REC
%token EXCEPTION
%token OF
%token TRY
%token IN
%token IF
%token THEN
%token ELSE
%token FUN
%token MAPSTO
%token <int> INT
%token <string> VAR
%token <string> CONSTR
%token <string> INFIXOP
%token UNIT
%token TRUE
%token FALSE
%token NOT
%token COMMA
%token INL
%token INR
%token MATCH
%token WITH
%token CASE
%token LEFT_BRACK
%token RIGHT_BRACK
%token EQ
%token EOF

%type <mlterm> term prog
%type <constructor * identifier * mlterm> tryblock
%type <qf_mltype> qf_type

%start prog

%%

prog:
    | t = term; EOF { t }

tryblock:
    | CASE; c = CONSTR;  v = VAR; MAPSTO; t = term { (c, v, t) }

qf_type:
    | t = BASETYPE; { Basetype t }
    | LEFT_BRACK; t1 = qf_type; MAPSTO; t2 = qf_type; RIGHT_BRACK { Arrowtype (t1, t2) }
    | LEFT_BRACK; t1 = qf_type; op = INFIXOP; t2 = qf_type; RIGHT_BRACK { if op = "*" then Prod (t1, t2) else raise (Failure "Invalid type") }

term:
    | LET; v = VAR; args = list(VAR); EQ; t1 = term; IN; t2 = term { Let (v, List.fold_right (fun e acc -> Fun (e, acc)) args t1, t2) }
    | LET; REC; v = VAR; args = list(VAR); EQ; t1 = term; IN; t2 = term { LetRec (v, List.fold_right (fun e acc -> Fun (e, acc)) args t1, t2) }
    | LET; EXCEPTION; v = CONSTR; OF; tp = qf_type; IN; t = term { LetExc (v, tp, t) }
    | v = VAR { Var v }
    | i = INT { Int i }
    | c = CONSTR { Constr c }
    | TRUE { Bool true }
    | FALSE { Bool false }
    | UNIT { Unit }
    | FUN; l = nonempty_list(VAR); MAPSTO; t = term { List.fold_right (fun e acc -> Fun (e, acc)) l t }
    | LEFT_BRACK; t1 = term; t2 = term; RIGHT_BRACK { App (t1, t2) }
    | LEFT_BRACK; t1 = term; op = INFIXOP; t2 = term; RIGHT_BRACK { App (App ((Var op), t1), t2) }
    | LEFT_BRACK; IF; c = term; THEN; t = term; ELSE; e = term; RIGHT_BRACK { ITE (c, t, e) }
    | LEFT_BRACK; NOT; t = term; RIGHT_BRACK; { App (Var "not", t) }
    | LEFT_BRACK; t1 = term; COMMA; t2 = term; RIGHT_BRACK; { Pair (t1, t2) }
    | LEFT_BRACK; INL; t = term; RIGHT_BRACK { App (Var "inl", t) }
    | LEFT_BRACK; INR; t = term; RIGHT_BRACK { App (Var "inr", t) }
    | LEFT_BRACK; MATCH; t = term; WITH; CASE; INL v1 = VAR; MAPSTO; t1 = term; CASE; INR; v2 = VAR; MAPSTO; t2 = term; RIGHT_BRACK { Match (t, v1, t1, v2, t2) }
    | LEFT_BRACK; TRY; t = term; WITH; l = list(tryblock); RIGHT_BRACK { Try (t, l) }
    | LEFT_BRACK; t = term; RIGHT_BRACK { t }
