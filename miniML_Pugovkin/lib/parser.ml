open Ast
open Angstrom

exception Error of string

(* монодический сахар *)
let ( let* ) = ( >>= )
let ( let+ ) = ( >>| )
let many1 p = lift2 (fun x xs -> x :: xs) p (many p)

(* ---------- утилиты AST ---------- *)

let mk_lams (params : string list) (body : expr) : expr =
  List.fold_right (fun x acc -> Lam (x, acc)) params body

(* ---------- пробелы и комментарии ---------- *)

let is_space = function ' ' | '\t' | '\r' | '\n' -> true | _ -> false
let is_ident_start = function 'a'..'z' | 'A'..'Z' | '_' -> true | _ -> false
let is_ident_char  = function
  | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '\'' -> true
  | _ -> false
let is_digit = function '0'..'9' -> true | _ -> false

let skip_while1 p = satisfy p *> skip_while p

(* вложенные комментарии:  (*  ...  *)  *)
let rec comment () : unit Angstrom.t =
  Angstrom.string "(*" *>
  Angstrom.fix (fun loop ->
    Angstrom.choice
      [ Angstrom.string "*)" *> Angstrom.return ()
      ; comment () *> loop
      ; Angstrom.any_char *> loop
      ])

(* корректный пропуск: за шаг потребляет хотя бы что-то *)
let spaces : unit Angstrom.t =
  let ws1 =
    (skip_while1 is_space *> Angstrom.return ())
    <|> (comment ())
  in
  Angstrom.fix (fun loop ->
    (ws1 *> loop) <|> Angstrom.return ())

let lexeme p = p <* spaces
let sym s = lexeme (string s)

(* ключевое слово: не допускаем хвост идентификатора после него *)
let kw s =
  lexeme
    (string s *>
     peek_char >>= function
     | Some c when is_ident_char c -> fail ("keyword "^s^" is a prefix")
     | _ -> return ())

let reserved =
  [ "if"; "then"; "else"; "let"; "rec"; "in"
  ; "fun"; "not"; "true"; "false"
  ]

let not_reserved s =
  if List.mem s reserved then fail ("reserved word: " ^ s) else return s

(* ---------- атомы ---------- *)

let p_int  : expr Angstrom.t =
  lexeme (take_while1 is_digit >>| int_of_string >>| fun n -> Const (Int n))

let p_bool : expr Angstrom.t =
  choice [ kw "true"  *> return (Const (Bool true))
         ; kw "false" *> return (Const (Bool false)) ]

let p_ident : string Angstrom.t =
  lexeme
    (lift2 (fun c cs -> String.make 1 c ^ cs)
       (satisfy is_ident_start)
       (take_while is_ident_char))
  >>= not_reserved

let p_var : expr Angstrom.t = p_ident >>| fun x -> Var x

(* ---------- выражения (через fix) ---------- *)

let p_expr : expr Angstrom.t =
  fix (fun expr ->
    (* () | (expr) *)
    let p_parens : expr Angstrom.t =
      sym "(" *>
      choice
        [ sym ")" *> return (Const (Unit()))
        ; expr <* sym ")"
        ]
    in

    let p_atom : expr Angstrom.t = choice [ p_int; p_bool; p_var; p_parens ] in

    (* application: f a b *)
    let p_apply : expr Angstrom.t =
      let* f = p_atom in
      let+ args = many p_atom in
      List.fold_left (fun acc a -> App (acc, a)) f args
    in

    (* unary — поверх application: -f x == -(f x) *)
    let p_unary : expr Angstrom.t =
      choice
        [ sym "-"  *> (p_apply >>| fun e -> Unop (UMinus, e))
        ; sym "+"  *> (p_apply >>| fun e -> Unop (UPlus,  e))
        ; kw  "not"*> (p_apply >>| fun e -> Unop (Not,    e))
        ; p_apply
        ]
    in

    let chainl1 p op =
      let* x = p in
      let rec rest acc =
        (let* f = op in
         let* y = p in
         rest (f acc y))
        <|> return acc
      in
      rest x
    in

    let p_mul : expr Angstrom.t =
      let op =
        choice
          [ sym "*" *> return (fun l r -> BinopArithmetic (Mul, l, r))
          ; sym "/" *> return (fun l r -> BinopArithmetic (Div, l, r))
          ]
      in
      chainl1 p_unary op
    in

    let p_add : expr Angstrom.t =
      let op =
        choice
          [ sym "+" *> return (fun l r -> BinopArithmetic (Add, l, r))
          ; sym "-" *> return (fun l r -> BinopArithmetic (Sub, l, r))
          ]
      in
      chainl1 p_mul op
    in

    let p_cmp : expr Angstrom.t =
      let* a = p_add in
      let op =
        choice
          [ sym "="  *> return (fun l r -> BinopComp (Eq,  l, r))
          ; sym "<>" *> return (fun l r -> BinopComp (Neq, l, r))
          ; sym "<=" *> return (fun l r -> BinopComp (Le,  l, r))
          ; sym ">=" *> return (fun l r -> BinopComp (Ge,  l, r))
          ; sym "<"  *> return (fun l r -> BinopComp (Lt,  l, r))
          ; sym ">"  *> return (fun l r -> BinopComp (Gt,  l, r))
          ]
      in
      (let* f = op in
       let+ b = p_add in
       f a b)
      <|> return a
    in

    let p_if : expr Angstrom.t =
      kw "if" *>
      let* c  = expr in
      kw "then" *>
      let* t  = expr in
      (kw "else" *> (expr >>| fun e -> If (c, t, Some e)))
      <|> return (If (c, t, None))
    in

    let p_fun : expr Angstrom.t =
      kw "fun" *>
      let* params = many1 p_ident <|> fail "Expected at least one parameter after 'fun'" in
      sym "->" *>
      let+ body = expr in
      mk_lams params body
    in

    let p_let_expr : expr Angstrom.t =
      kw "let" *>
      let* rf    = (kw "rec" *> return Rec) <|> return Nonrec in
      let* name  = p_ident in
      let* params= many p_ident in
      sym "=" *>
      let* rhs   = expr in
      let  rhs   = mk_lams params rhs in
      kw "in" *>
      let+ body  = expr in
      Let (rf, name, rhs, body)
    in

    spaces *> choice [ p_let_expr; p_if; p_fun; p_cmp ]
  )

(* ---------- top-level и программа ---------- *)

let sep : unit Angstrom.t = choice [ sym ";;" *> return (); sym ";" *> return () ]

let p_toplevel : toplevel Angstrom.t =
  (* сначала пробуем выражение (обработает let ... in ...),
     если не вышло — это let без in *)
  (p_expr >>| fun e -> TExpr e)
  <|>
  (kw "let" *>
   let* rf     = (kw "rec" *> return Rec) <|> return Nonrec in
   let* name   = p_ident in
   let* params = many p_ident in
   sym "=" *>
   let+ rhs    = p_expr in
   TLet (rf, name, mk_lams params rhs))

let p_program : program Angstrom.t =
  spaces *> many (p_toplevel <* many sep) <* end_of_input

(* ---------- API ---------- *)

let run_exn p s =
  match parse_string ~consume:All p s with
  | Ok v -> v
  | Error msg -> raise (Error msg)

let expr_of_string s =
  run_exn (spaces *> p_expr <* option () sep <* end_of_input) s

let toplevel_of_string s =
  run_exn (spaces *> p_toplevel <* many sep <* end_of_input) s

let program_of_string s =
  run_exn p_program s
