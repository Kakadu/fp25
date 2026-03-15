(* qt_tests.ml *)
(* TODO: refactor + add to README *)

open C_sharp_strange_lib
open QCheck
open Gen
open Ast

(* =========================================================================== *)
(* Basic generators *)
(* =========================================================================== *)

(* Basic characters and strings *)
let gen_alpha = Gen.(oneof [ char_range 'a' 'z'; char_range 'A' 'Z' ])
let gen_digit = Gen.char_range '0' '9'
let gen_ident_char = Gen.(oneof [ gen_alpha; gen_digit; return '_' ])

(* Generator for safe string characters *)
let gen_safe_char =
  let safe_chars =
    [ 'a'
    ; 'b'
    ; 'c'
    ; 'd'
    ; 'e'
    ; 'f'
    ; 'g'
    ; 'h'
    ; 'i'
    ; 'j'
    ; 'k'
    ; 'l'
    ; 'm'
    ; 'n'
    ; 'o'
    ; 'p'
    ; 'q'
    ; 'r'
    ; 's'
    ; 't'
    ; 'u'
    ; 'v'
    ; 'w'
    ; 'x'
    ; 'y'
    ; 'z'
    ; 'A'
    ; 'B'
    ; 'C'
    ; 'D'
    ; 'E'
    ; 'F'
    ; 'G'
    ; 'H'
    ; 'I'
    ; 'J'
    ; 'K'
    ; 'L'
    ; 'M'
    ; 'N'
    ; 'O'
    ; 'P'
    ; 'Q'
    ; 'R'
    ; 'S'
    ; 'T'
    ; 'U'
    ; 'V'
    ; 'W'
    ; 'X'
    ; 'Y'
    ; 'Z'
    ; '0'
    ; '1'
    ; '2'
    ; '3'
    ; '4'
    ; '5'
    ; '6'
    ; '7'
    ; '8'
    ; '9'
    ; ' '
    ; '_'
    ; '-'
    ; '+'
    ; '*'
    ; '/'
    ; '='
    ; '('
    ; ')'
    ; '.'
    ; ','
    ; ';'
    ; ':'
    ; '!'
    ; '?'
    ]
  in
  Gen.oneof (List.map return safe_chars)
;;

(* Char generator *)
let gen_printable_char =
  let printable_chars =
    [ 'a'
    ; 'b'
    ; 'c'
    ; 'd'
    ; 'e'
    ; 'f'
    ; 'g'
    ; 'h'
    ; 'i'
    ; 'j'
    ; 'k'
    ; 'l'
    ; 'm'
    ; 'n'
    ; 'o'
    ; 'p'
    ; 'q'
    ; 'r'
    ; 's'
    ; 't'
    ; 'u'
    ; 'v'
    ; 'w'
    ; 'x'
    ; 'y'
    ; 'z'
    ; 'A'
    ; 'B'
    ; 'C'
    ; 'D'
    ; 'E'
    ; 'F'
    ; 'G'
    ; 'H'
    ; 'I'
    ; 'J'
    ; 'K'
    ; 'L'
    ; 'M'
    ; 'N'
    ; 'O'
    ; 'P'
    ; 'Q'
    ; 'R'
    ; 'S'
    ; 'T'
    ; 'U'
    ; 'V'
    ; 'W'
    ; 'X'
    ; 'Y'
    ; 'Z'
    ; '0'
    ; '1'
    ; '2'
    ; '3'
    ; '4'
    ; '5'
    ; '6'
    ; '7'
    ; '8'
    ; '9'
    ]
  in
  Gen.oneof (List.map Gen.return printable_chars)
;;

(* Identifier generator with reserved words check *)
let reserved =
  [ "true"
  ; "false"
  ; "if"
  ; "else"
  ; "while"
  ; "public"
  ; "static"
  ; "void"
  ; "string"
  ; "char"
  ; "int"
  ; "bool"
  ; "for"
  ; "null"
  ; "new"
  ; "return"
  ; "break"
  ; "continue"
  ; "class"
  ; "async"
  ; "await"
  ]
;;

let gen_ident =
  let gen_first_char = Gen.char_range 'a' 'z' in
  let gen_rest = Gen.list_size (Gen.int_bound 8) gen_ident_char in
  let gen_name =
    Gen.map2
      (fun first rest ->
         let chars = first :: rest in
         let buf = Buffer.create 10 in
         List.iter (Buffer.add_char buf) chars;
         Buffer.contents buf)
      gen_first_char
      gen_rest
  in
  Gen.map (fun name -> if List.mem name reserved then Id "x" else Id name) gen_name
;;

(* =========================================================================== *)
(* Types and values *)
(* =========================================================================== *)

let gen_base_type =
  Gen.oneof [ return TypeInt; return TypeChar; return TypeBool; return TypeString ]
;;

let gen_full_type =
  Gen.oneof [ Gen.map (fun bt -> TypeBase bt) gen_base_type; return TypeVoid ]
;;

let gen_modifier = Gen.oneof [ return MPublic; return MStatic; return MAsync ]
let gen_modifiers = Gen.(list_size (0 -- 2) gen_modifier)

(* =========================================================================== *)
(* Expression generator with type support *)
(* =========================================================================== *)

type type_env =
  { variables : (ident * _type) list
  ; functions : (ident * (_type list * _type)) list
  ; depth : int
  }

let empty_env = { variables = []; functions = []; depth = 3 }

let gen_safe_string =
  Gen.map
    (fun chars -> String.of_seq (List.to_seq chars))
    (Gen.list_size (Gen.int_bound 10) gen_safe_char)
;;

(* =========================================================================== *)
(* Expression generator (full, including assignments) *)
(* =========================================================================== *)

let rec gen_expr env expected_type =
  if env.depth <= 0
  then gen_base_expr env expected_type
  else (
    let env' = { env with depth = env.depth - 1 } in
    match expected_type with
    | TypeVoid ->
      (* For void with non-zero depth try calling functions *)
      (match env.functions with
       | [] -> gen_base_expr env expected_type
       | _ -> gen_funcall env' TypeVoid)
    | TypeBase _ ->
      Gen.oneof_weighted
        [ 3, gen_binop_expr env' expected_type
        ; 2, gen_unop_expr env' expected_type
        ; 2, gen_funcall env' expected_type
        ; 1, gen_id_expr env expected_type
        ])

and gen_expr_no_assign env expected_type =
  if env.depth <= 0
  then gen_base_expr_no_assign env expected_type
  else (
    let env' = { env with depth = env.depth - 1 } in
    match expected_type with
    | TypeVoid ->
      (* For void with non-zero depth try calling functions *)
      (match env.functions with
       | [] -> gen_base_expr_no_assign env expected_type
       | _ -> gen_funcall env' TypeVoid)
    | TypeBase _ ->
      Gen.oneof_weighted
        [ 3, gen_binop_expr_no_assign env' expected_type
        ; 2, gen_unop_expr_no_assign env' expected_type
        ; 2, gen_funcall env' expected_type
        ; 1, gen_id_expr_no_assign env expected_type
        ])

(* Base expressions for shallow depth *)
and gen_base_expr env = function
  | TypeBase TypeInt ->
    Gen.oneof_weighted
      [ 4, Gen.map (fun i -> EValue (ValInt i)) (int_bound 100)
      ; 1, Gen.map (fun id -> EId id) gen_ident
      ]
  | TypeBase TypeBool ->
    Gen.oneof_weighted
      [ 4, Gen.map (fun b -> EValue (ValBool b)) bool
      ; 1, Gen.map (fun id -> EId id) gen_ident
      ]
  | TypeBase TypeChar ->
    Gen.oneof_weighted
      [ 4, Gen.map (fun c -> EValue (ValChar c)) gen_printable_char
      ; 1, Gen.map (fun id -> EId id) gen_ident
      ]
  | TypeBase TypeString ->
    Gen.oneof_weighted
      [ 4, Gen.map (fun s -> EValue (ValString s)) gen_safe_string
      ; 1, Gen.map (fun id -> EId id) gen_ident
      ]
  | TypeVoid ->
    (* For void always generate assignment *)
    Gen.(
      gen_base_type
      >>= fun typ ->
      gen_ident
      >>= fun id ->
      let var_type = TypeBase typ in
      gen_expr_no_assign env var_type >>= fun e -> return (EBinOp (OpAssign, EId id, e)))

(* Identifier generator with type awareness *)
and gen_id_expr env expected_type =
  match expected_type with
  | TypeBase _ ->
    let vars_of_type = List.filter (fun (_, t) -> t = expected_type) env.variables in
    (match vars_of_type with
     | [] ->
       (* If no variables of required type, generate a new one with correct type *)
       Gen.map (fun id -> EId id) gen_ident
     | vars ->
       let var_ids = List.map fst vars in
       Gen.oneof (List.map (fun id -> Gen.return (EId id)) var_ids))
  | TypeVoid ->
    (* For void always generate assignment *)
    Gen.(
      gen_base_type
      >>= fun typ ->
      gen_ident
      >>= fun id ->
      let var_type = TypeBase typ in
      gen_expr_no_assign env var_type >>= fun e -> return (EBinOp (OpAssign, EId id, e)))

(* Binary operations *)
and gen_binop_expr env expected_type =
  match expected_type with
  | TypeBase TypeInt ->
    let int_ops = [ OpAdd; OpSub; OpMul; OpDiv; OpMod ] in
    let op_gen = Gen.oneof (List.map return int_ops) in
    Gen.map3
      (fun op l r -> EBinOp (op, l, r))
      op_gen
      (gen_expr env (TypeBase TypeInt))
      (gen_expr env (TypeBase TypeInt))
  | TypeBase TypeBool ->
    let comparison_ops =
      [ OpEqual; OpNonEqual; OpLess; OpMore; OpLessEqual; OpMoreEqual ]
    in
    let logical_ops = [ OpAnd; OpOr ] in
    Gen.oneof_weighted
      [ ( 2
        , Gen.map3
            (fun op l r -> EBinOp (op, l, r))
            (Gen.oneof (List.map return comparison_ops))
            (gen_expr env (TypeBase TypeInt))
            (gen_expr env (TypeBase TypeInt)) )
      ; ( 1
        , Gen.map3
            (fun op l r -> EBinOp (op, l, r))
            (Gen.oneof (List.map return logical_ops))
            (gen_expr env (TypeBase TypeBool))
            (gen_expr env (TypeBase TypeBool)) )
      ]
  | TypeBase TypeChar | TypeBase TypeString ->
    (* For char and string only comparison *)
    let comp_ops = [ OpEqual; OpNonEqual ] in
    Gen.map3
      (fun op l r -> EBinOp (op, l, r))
      (Gen.oneof (List.map return comp_ops))
      (gen_expr env expected_type)
      (gen_expr env expected_type)
  | TypeVoid -> gen_expr env TypeVoid

(* Unary operations *)
and gen_unop_expr env expected_type =
  match expected_type with
  | TypeBase TypeBool ->
    Gen.map2 (fun op e -> EUnOp (op, e)) (return OpNot) (gen_expr env (TypeBase TypeBool))
  | TypeBase TypeInt ->
    Gen.oneof_weighted
      [ ( 2
        , Gen.map2
            (fun op e -> EUnOp (op, e))
            (return OpNeg)
            (gen_expr env (TypeBase TypeInt)) )
      ; ( 1
        , Gen.map2
            (fun op id -> EUnOp (op, EId id))
            (Gen.oneof [ return OpNot; return OpNeg ])
            gen_ident )
      ]
  | _ -> gen_expr env expected_type

(* Function calls *)
and gen_funcall env expected_type =
  let funcs_of_type =
    List.filter (fun (_, (_, ret)) -> ret = expected_type) env.functions
  in
  match funcs_of_type with
  | [] -> gen_base_expr env expected_type
  | funcs_list ->
    let gen_func = Gen.oneof (List.map (fun (id, _) -> return (EId id)) funcs_list) in
    Gen.map2
      (fun f args -> EFuncCall (f, Args args))
      gen_func
      (gen_args env expected_type)

and gen_args env expected_type =
  Gen.(int_bound 2 >>= fun count -> list_size (return count) (gen_expr env expected_type))

(* =========================================================================== *)
(* Expression generator WITHOUT assignments (for initializers) *)
(* =========================================================================== *)

(* Base expressions WITHOUT assignments for shallow depth *)
and gen_base_expr_no_assign _ = function
  | TypeBase TypeInt ->
    Gen.oneof_weighted
      [ 4, Gen.map (fun i -> EValue (ValInt i)) (int_bound 100)
      ; 1, Gen.map (fun id -> EId id) gen_ident
      ]
  | TypeBase TypeBool ->
    Gen.oneof_weighted
      [ 4, Gen.map (fun b -> EValue (ValBool b)) bool
      ; 1, Gen.map (fun id -> EId id) gen_ident
      ]
  | TypeBase TypeChar ->
    Gen.oneof_weighted
      [ 4, Gen.map (fun c -> EValue (ValChar c)) gen_printable_char
      ; 1, Gen.map (fun id -> EId id) gen_ident
      ]
  | TypeBase TypeString ->
    Gen.oneof_weighted
      [ 4, Gen.map (fun s -> EValue (ValString s)) gen_safe_string
      ; 1, Gen.map (fun id -> EId id) gen_ident
      ]
  | TypeVoid ->
    (* For void at zero depth - use existing variables
       or create new ones through declarations in gen_decl_stmt *)
    Gen.oneof
      [ Gen.map (fun id -> EId id) gen_ident
      ; Gen.map (fun i -> EValue (ValInt i)) (int_bound 100)
      ; Gen.map (fun b -> EValue (ValBool b)) bool
      ; Gen.map (fun c -> EValue (ValChar c)) gen_printable_char
      ; Gen.map (fun s -> EValue (ValString s)) gen_safe_string
      ]

(* Identifier generator with type awareness (no assignments) *)
and gen_id_expr_no_assign env expected_type =
  match expected_type with
  | TypeBase _ ->
    let vars_of_type = List.filter (fun (_, t) -> t = expected_type) env.variables in
    (match vars_of_type with
     | [] ->
       (* If no variables of required type, generate a value *)
       gen_base_expr_no_assign env expected_type
     | vars ->
       let var_ids = List.map fst vars in
       Gen.oneof (List.map (fun id -> Gen.return (EId id)) var_ids))
  | TypeVoid ->
    (* For void generate a value *)
    gen_base_expr_no_assign env TypeVoid

(* Unary operations without assignment *)
and gen_unop_expr_no_assign env expected_type =
  match expected_type with
  | TypeBase TypeBool ->
    Gen.map2
      (fun op e -> EUnOp (op, e))
      (return OpNot)
      (gen_expr_no_assign env (TypeBase TypeBool))
  | TypeBase TypeInt ->
    Gen.oneof_weighted
      [ ( 2
        , Gen.map2
            (fun op e -> EUnOp (op, e))
            (return OpNeg)
            (gen_expr_no_assign env (TypeBase TypeInt)) )
      ; ( 1
        , Gen.map2
            (fun op id -> EUnOp (op, EId id))
            (Gen.oneof [ return OpNot; return OpNeg ])
            gen_ident )
      ]
  | _ -> gen_expr_no_assign env expected_type

(* Binary operations without assignment *)
and gen_binop_expr_no_assign env expected_type =
  match expected_type with
  | TypeBase TypeInt ->
    let int_ops = [ OpAdd; OpSub; OpMul; OpDiv; OpMod ] in
    let op_gen = Gen.oneof (List.map return int_ops) in
    Gen.map3
      (fun op l r -> EBinOp (op, l, r))
      op_gen
      (gen_expr_no_assign env (TypeBase TypeInt))
      (gen_expr_no_assign env (TypeBase TypeInt))
  | TypeBase TypeBool ->
    let comparison_ops =
      [ OpEqual; OpNonEqual; OpLess; OpMore; OpLessEqual; OpMoreEqual ]
    in
    let logical_ops = [ OpAnd; OpOr ] in
    Gen.oneof_weighted
      [ ( 2
        , Gen.map3
            (fun op l r -> EBinOp (op, l, r))
            (Gen.oneof (List.map return comparison_ops))
            (gen_expr_no_assign env (TypeBase TypeInt))
            (gen_expr_no_assign env (TypeBase TypeInt)) )
      ; ( 1
        , Gen.map3
            (fun op l r -> EBinOp (op, l, r))
            (Gen.oneof (List.map return logical_ops))
            (gen_expr_no_assign env (TypeBase TypeBool))
            (gen_expr_no_assign env (TypeBase TypeBool)) )
      ]
  | TypeBase TypeChar | TypeBase TypeString ->
    let comp_ops = [ OpEqual; OpNonEqual ] in
    Gen.map3
      (fun op l r -> EBinOp (op, l, r))
      (Gen.oneof (List.map return comp_ops))
      (gen_expr_no_assign env expected_type)
      (gen_expr_no_assign env expected_type)
  | TypeVoid -> gen_expr_no_assign env TypeVoid
;;

(* =========================================================================== *)
(* Type for assignment result *)
(* =========================================================================== *)

type assign_result =
  | AssignExpr of expr
  | AssignBlock of stmt

(* =========================================================================== *)
(* Statement generator *)
(* =========================================================================== *)

let rec gen_stmt env return_type =
  if env.depth <= 0
  then gen_simple_stmt env return_type
  else (
    let env' = { env with depth = env.depth - 1 } in
    Gen.oneof_weighted
      [ 2, gen_decl_stmt env'
      ; 2, gen_expr_stmt env'
      ; 2, gen_if_stmt env' return_type
      ; 2, gen_while_stmt env' return_type
      ; 1, gen_for_stmt env' return_type
      ; 1, gen_return_stmt env' return_type
      ; 1, gen_block_stmt env' return_type
      ; 1, gen_break_continue_stmt
      ])

and gen_simple_stmt env return_type =
  Gen.oneof [ gen_decl_stmt env; gen_expr_stmt env; gen_return_stmt env return_type ]

and gen_decl_stmt env =
  Gen.(
    gen_base_type
    >>= fun typ ->
    gen_ident
    >>= fun id ->
    (* Generate initializer of correct type WITHOUT assignments *)
    let expr_gen =
      match typ with
      | TypeInt -> gen_expr_no_assign env (TypeBase TypeInt)
      | TypeChar -> gen_expr_no_assign env (TypeBase TypeChar)
      | TypeBool -> gen_expr_no_assign env (TypeBase TypeBool)
      | TypeString -> gen_expr_no_assign env (TypeBase TypeString)
    in
    (* Always generate an initializer *)
    expr_gen
    >>= fun init_expr ->
    (* Add variable to environment (for subsequent statements) *)
    let new_var = id, TypeBase typ in
    let _ = { env with variables = new_var :: env.variables } in
    return (SDecl (Var (TypeVar (TypeBase typ), id), Some init_expr)))

(* Assignment generator *)
and gen_assign_expr env : assign_result Gen.t =
  match env.variables with
  | [] ->
    (* If no variables, first create a declaration *)
    Gen.(
      gen_base_type
      >>= fun typ ->
      gen_ident
      >>= fun id ->
      let var_type = TypeBase typ in
      gen_expr env var_type
      >>= fun e ->
      return
        (AssignBlock
           (SBlock
              [ SDecl (Var (TypeVar var_type, id), Some e)
              ; SExpr (EBinOp (OpAssign, EId id, EId id))
              ])))
  | vars ->
    let var_ids = List.map fst vars in
    Gen.(
      oneof
        (List.map
           (fun id ->
              (* Choose appropriate type for expression *)
              let typ = List.assoc id env.variables in
              gen_expr env typ
              >>= fun e -> return (AssignExpr (EBinOp (OpAssign, EId id, e))))
           var_ids))

(* Function call generator with return value ignored *)
and gen_funcall_ignore_return env : expr Gen.t =
  let non_void_funcs = List.filter (fun (_, (_, ret)) -> ret <> TypeVoid) env.functions in
  match non_void_funcs with
  | [] ->
    gen_assign_expr env
    >>= (function
     | AssignExpr e -> Gen.return e
     | AssignBlock (SBlock stmts) ->
       (* Find expression in block *)
       let rec find_expr = function
         | [] -> gen_base_expr env (TypeBase TypeInt)
         | SExpr e :: _ -> Gen.return e
         | _ :: rest -> find_expr rest
       in
       find_expr stmts
     | _ -> gen_base_expr env (TypeBase TypeInt))
    (* fallback *)
  | funcs_list ->
    Gen.(
      oneof
        (List.map
           (fun (func_id, (param_types, _)) ->
              let rec gen_args_for_params params acc =
                match params with
                | [] -> return (List.rev acc)
                | t :: ts ->
                  (match t with
                   | TypeBase bt ->
                     gen_expr env (TypeBase bt)
                     >>= fun arg -> gen_args_for_params ts (arg :: acc)
                   | _ -> gen_args_for_params ts acc (* skip unsupported types *))
              in
              gen_args_for_params param_types []
              >>= fun args -> return (EFuncCall (EId func_id, Args args)))
           funcs_list))

(* Expression statements generator *)
and gen_expr_stmt env : stmt Gen.t =
  Gen.(
    oneof_weighted
      [ ( 4
        , gen_assign_expr env
          >>= function
          | AssignExpr e -> return (SExpr e)
          | AssignBlock stmt -> return stmt )
      ; (4, gen_funcall env TypeVoid >>= fun e -> return (SExpr e))
      ; ( 2
        , if
            List.length (List.filter (fun (_, (_, ret)) -> ret <> TypeVoid) env.functions)
            > 0
          then gen_funcall_ignore_return env >>= fun e -> return (SExpr e)
          else
            gen_assign_expr env
            >>= function
            | AssignExpr e -> return (SExpr e)
            | AssignBlock stmt -> return stmt )
      ])

and gen_if_stmt env return_type =
  Gen.map3
    (fun cond then_stmt else_stmt -> SIf (cond, then_stmt, else_stmt))
    (gen_expr env (TypeBase TypeBool))
    (gen_stmt env return_type)
    (Gen.option (gen_stmt env return_type))

and gen_while_stmt env return_type =
  Gen.map2
    (fun cond body -> SWhile (cond, body))
    (gen_expr env (TypeBase TypeBool))
    (gen_loop_body env return_type)

and gen_for_stmt env return_type =
  Gen.(
    option (gen_decl_stmt env)
    >>= fun init ->
    option (gen_expr env (TypeBase TypeBool))
    >>= fun cond ->
    (* Increment can only be ++ or -- operations *)
    (if cond <> None
     then
       oneof_weighted [ 2, option (gen_unop_expr env (TypeBase TypeInt)); 1, return None ]
     else return None)
    >>= fun incr ->
    gen_loop_body env return_type >>= fun body -> return (SFor (init, cond, incr, body)))

and gen_loop_body env return_type =
  Gen.oneof_weighted
    [ 3, gen_stmt env return_type; 1, return SBreak; 1, return SContinue ]

and gen_return_stmt env return_type =
  match return_type with
  | TypeVoid -> return (SReturn None)
  | _ -> Gen.map (fun e -> SReturn (Some e)) (gen_expr env return_type)

and gen_block_stmt env return_type =
  Gen.map (fun stmts -> SBlock stmts) (Gen.list_size (1 -- 3) (gen_stmt env return_type))

and gen_break_continue_stmt = Gen.oneof [ return SBreak; return SContinue ]

(* =========================================================================== *)
(* Class and program generators *)
(* =========================================================================== *)

let rec has_return = function
  | SReturn _ -> true
  | SBlock stmts -> List.exists has_return stmts
  | SIf (_, then_stmt, Some else_stmt) -> has_return then_stmt || has_return else_stmt
  | SIf (_, then_stmt, None) -> has_return then_stmt
  | SWhile (_, body) -> has_return body
  | SFor (_, _, _, body) -> has_return body
  | _ -> false
;;

let ensure_return stmt return_type =
  if return_type <> TypeVoid && not (has_return stmt)
  then (
    let default_return =
      match return_type with
      | TypeBase TypeInt -> SReturn (Some (EValue (ValInt 0)))
      | TypeBase TypeBool -> SReturn (Some (EValue (ValBool false)))
      | TypeBase TypeChar -> SReturn (Some (EValue (ValChar 'a')))
      | TypeBase TypeString -> SReturn (Some (EValue (ValString "")))
      | _ -> SReturn None
    in
    match stmt with
    | SBlock stmts -> SBlock (stmts @ [ default_return ])
    | _ -> SBlock [ stmt; default_return ])
  else if return_type = TypeVoid && not (has_return stmt)
  then (
    match stmt with
    | SBlock stmts -> SBlock stmts
    | _ -> stmt)
  else stmt
;;

let gen_param =
  Gen.map2 (fun typ id -> Var (TypeVar (TypeBase typ), id)) gen_base_type gen_ident
;;

let gen_method env depth =
  Gen.(
    gen_modifiers
    >>= fun modifiers ->
    gen_full_type
    >>= fun return_type ->
    gen_ident
    >>= fun id ->
    (* Generate parameters *)
    int_bound 3
    >>= fun param_count ->
    list_size (return param_count) gen_param
    >>= fun params ->
    (* Update environment with parameters *)
    let env_with_params =
      List.fold_left
        (fun env (Var (TypeVar t, id')) ->
           { env with variables = (id', t) :: env.variables })
        env
        params
    in
    (* Generate method body considering return type *)
    gen_stmt { env_with_params with depth } return_type
    >>= fun body ->
    (* Check and add return if needed *)
    let valid_body = ensure_return body return_type in
    return (Method (modifiers, return_type, id, Params params, valid_body)))
;;

let gen_field env =
  Gen.(
    gen_modifiers
    >>= fun modifiers ->
    gen_base_type
    >>= fun typ ->
    gen_ident
    >>= fun id ->
    (* Generate initializer of correct type WITHOUT assignments *)
    let expr_gen =
      match typ with
      | TypeInt -> gen_expr_no_assign { env with depth = 1 } (TypeBase TypeInt)
      | TypeChar -> gen_expr_no_assign { env with depth = 1 } (TypeBase TypeChar)
      | TypeBool -> gen_expr_no_assign { env with depth = 1 } (TypeBase TypeBool)
      | TypeString -> gen_expr_no_assign { env with depth = 1 } (TypeBase TypeString)
    in
    (* Always generate an initializer *)
    expr_gen
    >>= fun init -> return (VarField (modifiers, TypeVar (TypeBase typ), id, Some init)))
;;

let gen_class depth =
  let env = { empty_env with depth } in
  let rec build_members env acc count =
    if count <= 0
    then Gen.return (List.rev acc, env)
    else
      Gen.(
        bool
        >>= fun is_field ->
        if is_field
        then gen_field env >>= fun field -> build_members env (field :: acc) (count - 1)
        else
          gen_method env depth
          >>= fun meth ->
          match meth with
          | Method (_, ret_type, id, Params params, _) ->
            let param_types = List.map (fun (Var (TypeVar t, _)) -> t) params in
            let env' =
              { env with functions = (id, (param_types, ret_type)) :: env.functions }
            in
            build_members env' (meth :: acc) (count - 1)
          | _ -> build_members env (meth :: acc) (count - 1))
  in
  Gen.(
    int_range 1 3
    >>= fun member_count ->
    build_members env [] member_count
    >>= fun (members, _) ->
    gen_modifiers
    >>= fun modifiers -> gen_ident >>= fun id -> return (Class (modifiers, id, members)))
;;

let gen_program depth = Gen.map (fun cls -> Program cls) (gen_class depth)

(* =========================================================================== *)
(* Helper functions *)
(* =========================================================================== *)

let expr_to_code_string expr = Format.asprintf "%a" Prettyprinter.pp_expr expr
let program_to_code_string prog = Format.asprintf "%a" Prettyprinter.pp_prog prog

let compare_expr_structure e1 e2 =
  match e1, e2 with
  | EValue v1, EValue v2 ->
    (match v1, v2 with
     | ValInt _, ValInt _
     | ValChar _, ValChar _
     | ValBool _, ValBool _
     | ValString _, ValString _
     | ValNull, ValNull -> true
     | _ -> false)
  | EId _, EId _ -> true
  | EBinOp (op1, _, _), EBinOp (op2, _, _) -> op1 = op2
  | EUnOp (op1, _), EUnOp (op2, _) -> op1 = op2
  | EFuncCall (_, Args a1), EFuncCall (_, Args a2) -> List.length a1 = List.length a2
  | _ -> false
;;

(* =========================================================================== *)
(* QCheck generators *)
(* =========================================================================== *)

let test_count = 200

let expr_arbitrary depth =
  let env = { empty_env with depth } in
  let gen =
    Gen.oneof
      [ gen_expr env (TypeBase TypeInt)
      ; gen_expr env (TypeBase TypeBool)
      ; gen_expr env (TypeBase TypeChar)
      ; gen_expr env (TypeBase TypeString)
      ; gen_expr env TypeVoid
      ]
  in
  QCheck.make ~print:show_expr gen
;;

let program_arbitrary depth = QCheck.make ~print:show_program (gen_program depth)

(* =========================================================================== *)
(* Tests *)
(* =========================================================================== *)

let prop_roundtrip_expr =
  Test.make
    ~name:"Expression roundtrip: show -> parse -> show"
    ~count:test_count
    (expr_arbitrary 2)
    (fun expr ->
       let code_str = expr_to_code_string expr in
       match
         Angstrom.parse_string ~consume:Angstrom.Consume.All Parser.parse_ops code_str
       with
       | Ok expr' ->
         let code_str' = expr_to_code_string expr' in
         if code_str = code_str'
         then true
         else (
           Format.eprintf
             "\n\
              @[<v2>Expression roundtrip failed:@ Input AST: %s@ Output code: %s@ Parsed \
              back AST: %s@ Final code: %s@]"
             (show_expr expr)
             code_str
             (show_expr expr')
             code_str';
           false)
       | Error e ->
         Format.eprintf
           "\n@[<v2>Expression parse failed:@ Input AST: %s@ Output code: %s@ Error: %s@]"
           (show_expr expr)
           code_str
           e;
         false)
;;

let prop_roundtrip_program =
  Test.make
    ~name:"Program roundtrip: show -> parse -> show"
    ~count:test_count
    (program_arbitrary 1)
    (fun prog ->
       let code_str = program_to_code_string prog in
       match
         Angstrom.parse_string ~consume:Angstrom.Consume.All Parser.parse_prog code_str
       with
       | Ok prog' ->
         let code_str' = program_to_code_string prog' in
         if code_str = code_str'
         then true
         else (
           Format.eprintf
             "\n\
              @[<v2>Program roundtrip failed:@ Input AST: %s@ Output code: %s@ Parsed \
              back AST: %s@ Final code: %s@]"
             (show_program prog)
             code_str
             (show_program prog')
             code_str';
           false)
       | Error e ->
         Format.eprintf
           "\n@[<v2>Program parse failed:@ Input AST: %s@ Output code: %s@ Error: %s@]"
           (show_program prog)
           code_str
           e;
         false)
;;

let prop_operator_precedence =
  Test.make
    ~name:"Operator precedence is preserved"
    ~count:test_count
    (expr_arbitrary 2)
    (fun expr ->
       let code_str = expr_to_code_string expr in
       match
         Angstrom.parse_string ~consume:Angstrom.Consume.All Parser.parse_ops code_str
       with
       | Ok expr' ->
         if compare_expr_structure expr expr'
         then true
         else (
           Format.eprintf
             "\n\
              @[<v2>Precedence failed:@ Original AST: %s@ Original code: %s@ Parsed AST: \
              %s@ Types differ@]"
             (show_expr expr)
             code_str
             (show_expr expr');
           false)
       | Error e ->
         Format.eprintf
           "\n\
            @[<v2>Precedence test parse failed:@ Expression AST: %s@ Code: %s@ Error: \
            %s@]"
           (show_expr expr)
           code_str
           e;
         false)
;;

let prop_parse_errors =
  let gen_invalid =
    Gen.oneof
      [ return "??"
      ; return "+++"
      ; return "()"
      ; return ".;"
      ; return "==="
      ; return "!"
      ; return "&"
      ; return "|"
      ; return "*"
      ; return "/"
      ; return "class"
      ; return "if"
      ; return "while"
      ]
  in
  Test.make
    ~name:"Parser returns error on invalid input"
    ~count:test_count
    (QCheck.make ~print:(fun s -> s) gen_invalid)
    (fun str ->
       match Angstrom.parse_string ~consume:Angstrom.Consume.All Parser.parse_ops str with
       | Error _ -> true
       | Ok expr ->
         Format.eprintf
           "\n\
            @[<v2>Parser should have failed but succeeded:@ Input: %s@ Parsed as AST: \
            %s@]"
           str
           (show_expr expr);
         false)
;;

let prop_parse_no_crash =
  Test.make
    ~name:"Parser does not crash on valid expressions"
    ~count:test_count
    (expr_arbitrary 2)
    (fun expr ->
       let code_str = expr_to_code_string expr in
       match
         Angstrom.parse_string ~consume:Angstrom.Consume.All Parser.parse_ops code_str
       with
       | Ok _ -> true
       | Error e ->
         Format.eprintf
           "\n\
            @[<v2>Parser failed on valid expression (not a crash):@ Input AST: %s@ \
            Generated code: %s@ Error: %s@]"
           (show_expr expr)
           code_str
           e;
         true)
;;

(* =========================================================================== *)
(* Test runner with command line argument handling *)
(* =========================================================================== *)

let tests =
  [ prop_parse_no_crash
  ; prop_roundtrip_expr
  ; prop_operator_precedence
  ; prop_roundtrip_program
  ; prop_parse_errors
  ]
;;

(* Function to run tests *)
let run () = QCheck_base_runner.run_tests ~verbose:true tests

(* Command line argument handling and execution *)
let () =
  Arg.parse
    [ "--seed", Arg.Int QCheck_base_runner.set_seed, " Set random seed" ]
    (fun _ -> ())
    "Usage: qt_tests.exe [options]";
  let exit_code = run () in
  if exit_code <> 0 then exit exit_code
;;

(* TODO: remove printf for debugging *)
