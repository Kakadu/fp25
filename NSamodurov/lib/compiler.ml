open Ast

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
[@@deriving show { with_path = false }]

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
  | Dummy (** Push dummy symbol to enviroment, used by let rec *)
  | Update (**  *)
[@@deriving show { with_path = false }]

let rec list_of_apps = function
  | EApp (a1, a2) -> a1 :: list_of_apps a2
  | a -> [ a ]
;;

(* TODO: write CPS version *)
let compile =
  let helper_op f l instr =
    match l with
    | EVar (Index 0) :: tl -> f (Primitive Add) tl
    | EVar (Index 1) :: tl -> f (Primitive Sub) tl
    | EVar (Index 2) :: tl -> f (Primitive Mul) tl
    | EVar (Index 3) :: tl -> f (Primitive Div) tl
    | EVar (Index 4) :: tl -> f (Primitive Less) tl
    | EVar (Index 5) :: tl -> f (Primitive Great) tl
    | EVar (Index 6) :: tl -> f (Primitive LessEq) tl
    | EVar (Index 7) :: tl -> f (Primitive GreatEq) tl
    | EVar (Index 8) :: tl -> f (Primitive Equal) tl
    | EVar (Index 9) :: tl -> f (Primitive NeqPhysical) tl
    | EVar (Index 10) :: tl -> f (Primitive NeqStruct) tl
    | EVar (Index 11) :: tl -> f (Primitive And) tl
    | EVar (Index 12) :: tl -> f (Primitive Or) tl
    | apps -> f instr apps
  in
  let rec helper_t acc = function
    | EVar (Index i) -> Access (i - reserved) :: acc
    | EIf (pred, e1, e2) ->
      let then_instr = helper_t [] e1 in
      let else_instr = helper_t [] e2 in
      let pred_instr = helper_t [] pred in
      let then_ofs = List.length then_instr in
      let else_ofs = List.length else_instr in
      pred_instr
      @ [ BranchIf (then_ofs + 1) ]
      @ then_instr
      @ [ Branch else_ofs ]
      @ else_instr
      @ acc
    | EApp (e1, e2) ->
      let aux instr l =
        helper_c
          (List.fold_left (fun acc x -> Push :: helper_c acc x) (instr :: acc) l)
          e2
      in
      helper_op aux (list_of_apps e1) AppTerm
    | EAbs (_, e) -> Grab :: helper_t acc e
    | ELet (Recursive, _, a, b) -> Dummy :: helper_c (Update :: helper_t acc b) a
    | ELet (NotRecursive, _, a, b) -> helper_c (Let :: helper_t acc b) a
    | EConst (Int c) -> Const c :: acc
    | EConst (Bool c) -> Const (if c then 1 else 0) :: acc
  and helper_c acc = function
    | EVar (Index i) -> Access (i - reserved) :: acc
    | EIf (pred, e1, e2) ->
      (* TODO: rewrite a faster version *)
      let then_instr = helper_t [] e1 in
      let else_instr = helper_t [] e2 in
      let pred_instr = helper_t [] pred in
      let then_ofs = List.length then_instr in
      let else_ofs = List.length else_instr in
      pred_instr
      @ [ BranchIf (then_ofs + 1) ]
      @ then_instr
      @ [ Branch else_ofs ]
      @ else_instr
      @ acc
    | EApp (e1, e2) ->
      let aux instr l =
        helper_c
          (List.fold_left (fun acc x -> Push :: helper_c acc x) (instr :: acc) l)
          e2
      in
      (match list_of_apps e1 with
       | EVar (Index i) :: _ as apps when i < reserved -> helper_op aux apps Apply
       | apps -> PushMark :: helper_op aux apps Apply)
    | EAbs (_, e) -> Cur (helper_t [ Return ] e) :: acc
    | ELet (Recursive, _, a, b) ->
      Dummy :: helper_c (Update :: helper_c (EndLet :: acc) b) a
    | ELet (NotRecursive, _, a, b) -> helper_c (Let :: helper_c (EndLet :: acc) b) a
    | EConst (Int c) -> Const c :: acc
    | EConst (Bool c) -> Const (if c then 1 else 0) :: acc
  in
  helper_c []
;;
