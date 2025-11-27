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
  | Access of int (** Access n'th variable from enviroment stack *)
  | Cur of instr list (** Non-tail recursive closure *)
  | Const of int (** Put a constant to accumulator *)
  | Primitive of op (** Primitive operators *)
  | BranchIf of int (** Jump to specific offset if acc != 0 *)
  | Branch of int (** Unconditional jump *)
  | EndLet (** End of let "frame" *)
  | Return (** Return value and get to previous enviroment *)
  | Grab (** Get argument from argument stack *)
  | Apply (** Apply argument from accumulator *)
  | Let (** Start let "frame" *)
  | Push (** Push argument onto argument stack *)
  | PushMark (** Start marker of application sequence *)
  | AppTerm (** Tail-recursive version of EndLet *)
  | Dummy (** Push dummy symbol to enviroment, used by let rec *)
  | Update (** Replace the top of the enviroment stack  *)
[@@deriving show { with_path = false }]

let list_of_apps =
  let rec helper = function
    | EApp (a1, a2) -> a2 :: helper a1
    | a -> [ a ]
  in
  fun l -> List.rev (helper l)
;;

let compile : brujin t -> instr list =
  let helper_op f l instr =
    let op_arr =
      [| Primitive Add
       ; Primitive Sub
       ; Primitive Mul
       ; Primitive Div
       ; Primitive Less
       ; Primitive Great
       ; Primitive LessEq
       ; Primitive GreatEq
       ; Primitive Equal
       ; Primitive NeqPhysical
       ; Primitive NeqStruct
       ; Primitive And
       ; Primitive Or
      |]
    in
    match l with
    | EVar (Index i) :: tl when i >= -Array.length op_arr && i < 0 -> f op_arr.(-i - 1) tl
    | apps -> f instr apps
  in
  let rec helper_t acc = function
    | EVar (Index i) -> Access i :: acc
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
      (match list_of_apps e1 with
       | EVar (Index i) :: _ as apps when i < 0 -> helper_op aux apps AppTerm
       | apps -> PushMark :: helper_op aux apps AppTerm)
    | EAbs (_, e) -> Grab :: helper_t acc e
    | ELet (Recursive, _, a, b) -> Dummy :: helper_c (Update :: helper_t acc b) a
    | ELet (NotRecursive, _, a, b) -> helper_c (Let :: helper_t acc b) a
    | EConst (Int c) -> Const c :: acc
    | EConst (Bool c) -> Const (if c then 1 else 0) :: acc
  and helper_c acc = function
    | EVar (Index i) -> Access i :: acc
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
       | EVar (Index i) :: _ as apps when i < 0 -> helper_op aux apps Apply
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
