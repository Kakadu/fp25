(** Copyright 2025, Tenyaeva Ekaterina *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val arbitrary_structure : Tenyaeva_lib.Ast.structure QCheck.arbitrary
val test_round_trip : QCheck2.Test.t
