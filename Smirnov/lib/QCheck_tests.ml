open Mardukml_lib

let test_parser_after_print =
  QCheck.Test.make
    ~count:10000
    ~name:"parser_after_print"
    (QCheck.make (Ast.gen_mlterm_sized 10))
    (fun x ->
      try
        x
        |> Ast.mlterm_to_string
        |> Mardukml.parse
        |> fun y ->
        if x = y
        then true
        else (
          let () = Printf.printf "term failed: %s\n" (Ast.mlterm_to_string x) in
          false)
      with
      | _ ->
        let () = Printf.printf "Exception in %s\n" (Ast.mlterm_to_string x) in
        false)
;;

let () = QCheck_base_runner.run_tests_main [ test_parser_after_print ]
