type t =
  { id : int
  ; name : string
  }

type ident = t

let ident id name = { id; name }
let equal (a : ident) (b : ident) = a.id = b.id
let compare (a : ident) (b : ident) = Int.compare a.id b.id

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

  let find_by_id_opt id ((left, _) : _ t) = Id_map.find_opt id left
  let find_by_name_opt name ((_, right) : _ t) = Name_map.find_opt name right
  let fmap ~f (left, right) = Id_map.map f left, Name_map.map f right
  let fold_by_ids ~f ~init (left, _) = Id_map.fold f left init

  let fold2 ~f_id ~f_name (left, right) ~init_id ~init_name =
    Id_map.fold f_id left init_id, Name_map.fold f_name right init_name
  ;;
end

module Ident_map_m (M : Monads.STATE_MONAD) = struct
  open M

  let fmapm ~f (left, right) =
    let helper_id key data acc =
      let* acc = acc in
      let* data' = f data in
      return (Id_map.add key data' acc)
    in
    let* mapped_ids = Id_map.fold helper_id left (return Id_map.empty) in
    let helper_names key data acc =
      let* acc = acc in
      let* data' = f data in
      return (Name_map.add key data' acc)
    in
    let* mapped_names = Name_map.fold helper_names right (return Name_map.empty) in
    return (mapped_ids, mapped_names)
  ;;
end
