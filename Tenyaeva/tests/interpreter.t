(** Copyright 2025, Tenyaeva Ekaterina *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

  $ ../bin/REPL.exe <<EOF
  > 122
  - = 122

  $ ../bin/REPL.exe <<EOF
  > (08 + 122 / 2) * 3 - 4
  - = 203

  $ ../bin/REPL.exe <<EOF
  > -4 + 8
  - = 4

  $ ../bin/REPL.exe <<EOF
  > -4 + ()
  Type error
