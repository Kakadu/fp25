open Mardukml_lib

let test_parser_after_print =
  QCheck.Test.make
    ~count:100000
    ~name:"parser_after_print"
    (QCheck.make (Ast.gen_mlterm_sized 10))
    (fun x -> x |> Ast.mlterm_to_string |> Mardukml.parse = x)
;;

let () = QCheck_base_runner.run_tests_main [ test_parser_after_print ]
