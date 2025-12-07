[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Runs the pretty-printer on the input program string.
    On success returns formatted code;
    on parse error returns ["Parse error: <msg>"]. *)
val run_pp : string -> string
