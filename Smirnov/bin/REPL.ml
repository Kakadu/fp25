open Mardukml_lib

let repl = let _ = Printf.printf "MardukML REPL\n\n" in
    while true do
        try
            let _ = Printf.printf "# " in
            let s = read_line () in if String.trim s = "" then () else
            let res = Mardukml.interp s in
            Printf.printf "%s\n" (Lambda.lterm_to_string res)
        with
        | End_of_file -> let () = Printf.printf "\n" in exit 0
        | Failure s -> Printf.printf "%s\n%!" s
        | e -> Printf.printf "Unknown error: %s\n%!" (Printexc.to_string e)
    done
