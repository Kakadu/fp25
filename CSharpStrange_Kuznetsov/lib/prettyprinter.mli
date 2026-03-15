(** Copyright 2026, Dmitrii Kuznetsov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Format

(** [pp_list pp sep fmt lst] prints a list [lst], using [pp] to print
    elements and [sep] as separator between them *)
val pp_list : (formatter -> 'a -> unit) -> string -> formatter -> 'a list -> unit

(** [pp_option pp fmt opt] prints an optional value [opt], using [pp]
    to print the value if it exists *)
val pp_option : (formatter -> 'a -> unit) -> formatter -> 'a option -> unit

(** [pp_ident fmt id] prints an identifier *)
val pp_ident : formatter -> Ast.ident -> unit

(** [pp_base_type fmt bt] prints a base type (int, char, bool, string) *)
val pp_base_type : formatter -> Ast.base_type -> unit

(** [pp_type fmt t] prints a type (base type or void) *)
val pp_type : formatter -> Ast._type -> unit

(** [pp_var_type fmt vt] prints a variable type *)
val pp_var_type : formatter -> Ast.var_type -> unit

(** [pp_modifier fmt m] prints a modifier (public, static, async) *)
val pp_modifier : formatter -> Ast.modifier -> unit

(** [pp_var_decl fmt vd] prints a variable declaration *)
val pp_var_decl : formatter -> Ast.var_decl -> unit

(** [pp_bin_op fmt op] prints a binary operator *)
val pp_bin_op : formatter -> Ast.bin_op -> unit

(** [pp_un_op fmt op] prints a unary operator *)
val pp_un_op : formatter -> Ast.un_op -> unit

(** [pp_val_type fmt v] prints a literal value (number, character, null, bool, string) *)
val pp_val_type : formatter -> Ast.val_type -> unit

(** [pp_expr fmt e] prints an expression *)
val pp_expr : formatter -> Ast.expr -> unit

(** [pp_stmt fmt s] prints a statement *)
val pp_stmt : formatter -> Ast.stmt -> unit

(** [pp_sblock fmt stmts] prints a block of statements *)
val pp_sblock : formatter -> Ast.stmt list -> unit

(** [pp_field fmt f] prints a class field (variable or method) *)
val pp_field : formatter -> Ast.field -> unit

(** [pp_c_sharp_class fmt cls] prints a class definition *)
val pp_c_sharp_class : formatter -> Ast.c_sharp_class -> unit

(** [pp_prog fmt prog] prints a program (class) *)
val pp_prog : formatter -> Ast.program -> unit
