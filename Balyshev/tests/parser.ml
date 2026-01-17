open MiniML
open QCheck

module GenAst = struct
  open Parsetree

  let reduce f n = f (n / 2)

  let var_name =
    let open Gen in
    oneof [ oneofl [ "x"; "y"; "z"; "a"; "b"; "c" ] ]
  ;;

  let constant_constructor_name =
    let open Gen in
    oneof [ oneofl [ "None"; "Nothing"; "Nil" ] ]
  ;;

  let constructor_name =
    let open Gen in
    oneof [ oneofl [ "Some"; "Just"; "Box" ] ]
  ;;

  let binop =
    let open Gen in
    oneof
      [ return Add
      ; return Sub
      ; return Mul
      ; return Div
      ; return Eq
      ; return Ne
      ; return Gt
      ; return Lt
      ; return Le
      ; return Ge
      ]
  ;;

  let constant =
    let open Gen in
    oneof
      [ return CUnit
      ; map (fun i -> CInt i) (Gen.int_range 0 100)
      ; map (fun b -> CBool b) Gen.bool
      ]
  ;;

  let tuple gen make_tuple =
    let open Gen in
    map3 (fun a b xs -> make_tuple a b xs) gen gen (list_size (int_bound 1) gen)
  ;;

  let gen_expr_list gen =
    let open Gen in
    map
      (fun items ->
         Base.List.fold
           ~f:(fun acc item -> EConstruct ("::", Some (ETuple (item, acc, []))))
           ~init:(EConstruct ("[]", None))
           items)
      (list_size (int_bound 3) gen)
  ;;

  let gen_patt_list gen =
    let open Gen in
    map
      (fun items ->
         Base.List.fold
           ~f:(fun acc item -> PConstruct ("::", Some (PTuple (item, acc, []))))
           ~init:(PConstruct ("[]", None))
           items)
      (list_size (int_bound 3) gen)
  ;;

  let pattern =
    let open Gen in
    sized
    @@ fix (fun self n ->
      let atom =
        oneof
          [ return PAny
          ; map (fun s -> PVar s) var_name
          ; map (fun x -> PConstant x) constant
          ; map (fun name -> PConstruct (name, None)) constant_constructor_name
          ]
      in
      if n < 1
      then atom
      else
        oneof
          [ map2
              (fun name arg -> PConstruct (name, Some arg))
              constructor_name
              (self (n / 2))
          ; tuple (self (n / 4)) (fun x1 x2 xs -> PTuple (x1, x2, xs))
          ; gen_patt_list (self (n / 4))
          ])
  ;;

  let expression =
    let open Gen in
    sized
    @@ fix (fun self n ->
      let self = reduce self n in
      let atom =
        oneof [ map (fun c -> EConstant c) constant; map (fun v -> EVar v) var_name ]
      in
      if n < 1
      then atom
      else
        oneof
          [ atom
          ; tuple self (fun x1 x2 xs -> ETuple (x1, x2, xs))
          ; map3 (fun op l r -> EBinop (op, l, r)) binop self self
          ; map2 (fun p e -> EFun (p, e)) pattern self
          ; map3 (fun c t e -> EIf (c, t, e)) self self self
          ; map2 (fun f x -> EApp (f, x)) self self
          ; map2 (fun name arg -> EConstruct (name, Some arg)) constructor_name self
          ])
  ;;

  let expression =
    let open Gen in
    sized
    @@ fix (fun self n ->
      let atom =
        oneof
          [ map (fun c -> EConstant c) constant
          ; map (fun name -> EVar name) var_name
          ; map (fun name -> EConstruct (name, None)) constant_constructor_name
          ]
      in
      if n < 1
      then atom
      else
        oneof
          [ map2 (fun p e -> EFun (p, e)) pattern (self (n / 2))
          ; gen_expr_list (self (n / 4))
          ; map3 (fun op l r -> EBinop (op, l, r)) binop (self (n / 2)) (self (n / 2))
          ; tuple (self (n / 4)) (fun x1 x2 xs -> ETuple (x1, x2, xs))
          ])
  ;;
end

let shrink aux x = QCheck.Iter.(return x <+> aux x)

let rec shrink_pattern patt =
  let open QCheck.Iter in
  let open Parsetree in
  match patt with
  | PAny | PVar _ | PConstant _ -> empty
  | PTuple (p1, p2, ps) ->
    Base.List.fold
      (p1 :: p2 :: ps)
      ~f:(fun acc p -> acc <+> shrink shrink_pattern p)
      ~init:empty
  | PConstruct (_, None) -> empty
  | PConstruct (_, Some p) -> shrink shrink_pattern p
;;

let shrink_expression =
  let open QCheck.Iter in
  let open Parsetree in
  let rec self = function
    | EConstant _ | EVar _ | EConstruct (_, None) -> empty
    | ETuple (e1, e2, es) ->
      Base.List.fold (e1 :: e2 :: es) ~f:(fun acc e -> acc <+> shrink self e) ~init:empty
    | EBinop (_, l, r) -> shrink self l <+> shrink self r
    | EFun (p, e) ->
      map2 (fun p e -> EFun (p, e)) (shrink shrink_pattern p) (shrink self e)
    | EIf (c, t, e) -> shrink self c <+> shrink self t <+> shrink self e
    | EApp (f, x) -> shrink self f <+> shrink self x
    | EConstruct (_, Some e) -> shrink self e
    | ELet (_, (pe, pes), expr) ->
      Base.List.fold
        (pe :: pes)
        ~f:(fun acc (p, e) ->
          acc <+> map2 (fun p e -> EFun (p, e)) (shrink shrink_pattern p) (shrink self e))
        ~init:(shrink self expr)
    | EMatch (expr, (pe, pes)) ->
      Base.List.fold
        (pe :: pes)
        ~f:(fun acc (p, e) ->
          acc <+> map2 (fun p e -> EFun (p, e)) (shrink shrink_pattern p) (shrink self e))
        ~init:(shrink self expr)
  in
  self
;;

let test_expression =
  Test.make
    ~count:100
    ~name:"expession"
    ~max_gen:10
    (QCheck.make GenAst.expression)
    (fun expr ->
       Format.eprintf "\n<< %s >>\n\n" (Parsetree.show_expression expr);
       match Parser.parse_expression (Parsetree.show_expression expr) with
       | Ok parsed_expr when parsed_expr = expr -> true
       | Ok _ ->
         Format.eprintf "roundtrip mismatch on { %s }" (Parsetree.show_expression expr);
         false
       | Error _ ->
         Format.eprintf "error on { %s }" (Parsetree.show_expression expr);
         false)
;;

let test_pattern =
  Test.make
    ~count:100
    ~name:"pattern"
    ~max_gen:20
    (QCheck.make ~print:Parsetree.show_pattern ~shrink:shrink_pattern GenAst.pattern)
    (fun patt ->
       match Parser.parse_pattern (Parsetree.show_pattern patt) with
       | Ok rez when rez = patt -> true
       | Ok _ ->
         Format.eprintf
           "roundtrip mismatch on pattern { %s }"
           (Parsetree.show_pattern patt);
         false
       | Error _ ->
         Format.eprintf "parsing error on pattern { %s }" (Parsetree.show_pattern patt);
         false)
;;
