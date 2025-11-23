open Lambda_lib.Parser
open Lambda_lib.Print
open Lambda_lib.Inferencer

let () =
  let expr =
    match parse (In_channel.(input_all stdin) |> Base.String.rstrip) with
    | Ok expr ->
      let _ = Printf.printf "%s\n" (print_ast expr) in
      Ok expr
    | Error (`Parsing_error msg) -> Error msg
  in
  match expr with
  | Ok expr ->
    (match typecheck_program expr with
     | Ok ty -> Printf.printf "%s\n" (print_typ ty)
     | Error er -> Printf.printf "%s\n" (show_type_error er))
  | Error er -> Printf.printf "%s\n" er
;;
