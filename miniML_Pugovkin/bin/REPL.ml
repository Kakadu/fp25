open Lambda_lib

type opts =
  { mutable steps : int
  ; mutable dump_parsetree : bool
  }

let () =
  let opts = { steps = 10_000; dump_parsetree = false } in
  let () =
    let open Stdlib.Arg in
    parse
      [ "-steps", Int (fun n -> opts.steps <- n), "Max evaluation steps (default: 10000)"
      ; ( "-dparsetree"
        , Unit (fun () -> opts.dump_parsetree <- true)
        , "Dump parse tree, don't evaluate" )
      ]
      (fun _ ->
        Stdlib.Format.eprintf "Positional arguments are not supported\n%!";
        Stdlib.exit 1)
      "MiniML interpreter"
  in
  let input = In_channel.(input_all stdin) |> String.trim in
  if String.equal input ""
  then ()
  else (
    try
      let program = Parser.program_of_string input in
      if opts.dump_parsetree
      then Format.printf "Parsed: %s\n%!" (Ast.show_program program)
      else (
        match Interpret.eval_program ~fuel:opts.steps program with
        | Ok values ->
          List.iter (fun v -> print_endline (Interpret.string_of_value v)) values
        | Error err ->
          Printf.eprintf "Runtime error: %s\n%!" (Interpret.string_of_error err);
          Stdlib.exit 1)
    with
    | Parser.Error msg ->
      Printf.eprintf "Parse error: %s\n%!" msg;
      Stdlib.exit 1)
;;
