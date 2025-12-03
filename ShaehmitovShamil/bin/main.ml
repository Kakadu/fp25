open Interpreter

let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s
;;

let () =
  match Sys.argv with
  | [| _; filename |] ->
    let source_code =
      try read_file filename with
      | Sys_error msg ->
        Printf.eprintf "File error: %s\n" msg;
        exit 1
    in
    (match Parser.parse_structure_items source_code with
     | Ok program ->
       (match Interpreter.run_program program with
        | Ok _ -> ()
        | Error err -> Printf.eprintf "%s\n" (Interpreter.show_error err))
     | Error msg -> Printf.eprintf "Parse Error: %s\n" msg)
  | _ -> Printf.eprintf "Usage: %s <filename>\n" Sys.argv.(0)
;;
