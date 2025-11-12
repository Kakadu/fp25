type t =
  { id : int
  ; name : string
  }

type ident = t

let ident id name = { id; name }
let equal (a : ident) (b : ident) = a.id = b.id
let compare (a : ident) (b : ident) = Int.compare a.id b.id

module Ident_map = struct
  module Id_map = Stdlib.Map.Make (Int)
  module Name_map = Stdlib.Map.Make (String)

  type 'v t = 'v Id_map.t * 'v Name_map.t

  let empty : _ t = Id_map.empty, Name_map.empty

  let add { id; name } data ((left, right) : _ t) =
    Id_map.add id data left, Name_map.add name data right
  ;;

  let remove { id; name } (left, right) =
    Id_map.remove id left, Name_map.remove name right
  ;;

  let find_by_id_opt id ((left, _) : _ t) = Id_map.find_opt id left
  let find_by_name_opt name ((_, right) : _ t) = Name_map.find_opt name right
  let fmap ~f (left, right) = Id_map.map f left, Name_map.map f right
end
