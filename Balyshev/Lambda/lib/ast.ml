type name = string

type 'name t =
  | Var of 'name (** Variable [x] *)
  | Abs of 'name * 'name t (** Abstraction [λx.t] *)
  | App of 'name t * 'name t

let var x = Var x
let abs x l = Abs (x, l)
let app l r = App (l, r)
