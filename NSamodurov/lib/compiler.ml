open Ast

type instr =
  | Access of int (** Access variable *)
  | Cur of instr list (** Non-tail recursive closure *)
  | Const of int (** Put a constant to accumulator *)
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
  | Add (** Arithmetic operators  *)
  | Sub
  | Mul
  | Div
[@@deriving show { with_path = false }]

let rec list_of_apps = function
  | EApp (a1, a2) -> a1 :: list_of_apps a2
  | a -> [ a ]
;;

let compile =
  let rec helper_t acc = function
    | EVar (Index i) -> Access (i - reserved) :: acc
    | EIf (pred, e1, e2) ->
      let then_instr = helper_t [] e1 in
      let else_instr = helper_t [] e2 in
      let pred_instr = helper_t [] pred in
      let then_ofs = List.length then_instr in
      let else_ofs = List.length else_instr in
      pred_instr
      @ [ BranchIf then_ofs ]
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
       | EVar (Index 0) :: tl -> aux Add tl
       | EVar (Index 1) :: tl -> aux Sub tl
       | EVar (Index 2) :: tl -> aux Mul tl
       | EVar (Index 3) :: tl -> aux Div tl
       | apps -> aux AppTerm apps)
    | EAbs (_, e) -> Grab :: helper_t acc e
    | ELet (Recursive, _, a, b) -> Dummy :: helper_c (Update :: helper_t acc b) a
    | ELet (NotRecursive, _, a, b) -> helper_c (Let :: helper_t acc b) a
    | EConst (Int c) -> Const c :: acc
    | EConst (Bool c) -> Const (if c then 1 else 0) :: acc
  and helper_c acc = function
    | EVar (Index i) -> Access (i - reserved) :: acc
    | EIf (pred, e1, e2) ->
      (* inoptimized *)
      let then_instr = helper_t [] e1 in
      let else_instr = helper_t [] e2 in
      let pred_instr = helper_t [] pred in
      let then_ofs = List.length then_instr in
      let else_ofs = List.length else_instr in
      pred_instr
      @ [ BranchIf then_ofs ]
      @ then_instr
      @ [ Branch else_ofs ]
      @ else_instr
      @ acc
    | EApp (e1, e2) ->
      (match list_of_apps e1 with
       | EVar (Index 0) :: tl ->
         helper_c
           (List.fold_left (fun acc x -> Push :: helper_c acc x) (Add :: acc) tl)
           e2
       | apps ->
         PushMark
         :: helper_c
              (List.fold_left (fun acc x -> Push :: helper_c acc x) (Apply :: acc) apps)
              e2)
    | EAbs (_, e) -> Cur (helper_t [ Return ] e) :: acc
    | ELet (Recursive, _, a, b) ->
      Dummy :: helper_c (Update :: helper_c (EndLet :: acc) b) a
    | ELet (NotRecursive, _, a, b) -> helper_c (Let :: helper_c (EndLet :: acc) b) a
    | EConst (Int c) -> Const c :: acc
    | EConst (Bool c) -> Const (if c then 1 else 0) :: acc
  in
  helper_c []
;;
