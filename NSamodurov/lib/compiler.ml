open Ast

type instr =
  | Access of int
  | Cur of instr list
  | Const of int
  | EndLet
  | Return
  | Grab
  | Apply
  | Let
  | Push
  | PushMark
  | AppTerm
  | Dummy
  | Update
  | Add
[@@deriving show { with_path = false }]

let rec list_of_apps = function
  | EApp (a1, a2) -> a1 :: list_of_apps a2
  | a -> [ a ]
;;

let compile =
  let rec helper_t acc = function
    | EVar (Index i) -> Access i :: acc
    | EApp (e1, e2) ->
      (match list_of_apps e1 with
       | EVar (Index 0) :: tl ->
         helper_c
           (List.fold_left (fun acc x -> Push :: helper_c acc x) (Add :: acc) tl)
           e2
       | apps ->
         helper_c
           (List.fold_left (fun acc x -> Push :: helper_c acc x) (AppTerm :: acc) apps)
           e2)
    | EAbs (_, e) -> Grab :: helper_t acc e
    | ELet (Recursive, _, a, b) -> Dummy :: helper_c (Update :: helper_t acc b) a
    | ELet (NotRecursive, _, a, b) -> helper_c (Let :: helper_t acc b) a
    | EConst (Int c) -> Const c :: acc
  and helper_c acc = function
    | EVar (Index i) -> Access i :: acc
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
    | EConst (Int i) -> Const i :: acc
  in
  helper_c []
;;
