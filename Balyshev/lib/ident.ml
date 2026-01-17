type t =
  { id : int
  ; name : string
  }

let ident id name = { id; name }
let equal (a : t) (b : t) = a.id = b.id
let compare (a : t) (b : t) = Int.compare a.id b.id
let show ident = Format.sprintf "'ty%d" ident.id
let of_int n = ident n (Format.sprintf "'ty%d" n)

module Id_map = Stdlib.Map.Make (Int)
module Name_map = Stdlib.Map.Make (String)

module Ident_map = struct
  type 'v t = 'v Id_map.t * 'v Name_map.t

  let empty : _ t = Id_map.empty, Name_map.empty

  let add { id; name } data ((left, right) : _ t) =
    Id_map.add id data left, Name_map.add name data right
  ;;

  let remove { id; name } (left, right) =
    Id_map.remove id left, Name_map.remove name right
  ;;

  let find_by_id id ((left, _) : _ t) = Id_map.find_opt id left
  let find_by_name name ((_, right) : _ t) = Name_map.find_opt name right
  let fmap ~f (left, right) = Id_map.map f left, Name_map.map f right
  let fold ~f ~init (left, _) = Id_map.fold f left init
end
