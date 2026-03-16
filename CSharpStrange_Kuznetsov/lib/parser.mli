(** Copyright 2026, Dmitrii Kuznetsov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Parser for C#-like language *)

open Ast

(** {1 Basic parsers} *)

(** List of reserved keywords *)
val reserved : string list

(** Check if a string is a reserved keyword *)
val in_reserved : string -> bool

(** Check if character is a whitespace *)
val is_space : char -> bool

(** Check if character can be part of a token (letter, digit, underscore) *)
val is_token_sym : char -> bool

(** Parser that skips whitespace characters *)
val skip_spaces : unit Angstrom.t

(** [parens p] parses [p] enclosed in parentheses *)
val parens : 'a Angstrom.t -> 'a Angstrom.t

(** [braces p] parses [p] enclosed in curly braces *)
val braces : 'a Angstrom.t -> 'a Angstrom.t

(** [brackets p] parses [p] enclosed in square brackets *)
val brackets : 'a Angstrom.t -> 'a Angstrom.t

(** Skips zero or more semicolons *)
val skip_semicolons : unit Angstrom.t

(** Skips one or more semicolons *)
val skip_semicolons1 : unit Angstrom.t

(** {1 Value parsers} *)

(** Parses integer literals *)
val parse_int : val_type Angstrom.t

(** Parses character literals (e.g., 'a') *)
val parse_char : val_type Angstrom.t

(** Parses boolean literals (true/false) *)
val parse_bool : val_type Angstrom.t

(** Parses string literals (e.g., "hello") *)
val parse_val_string : val_type Angstrom.t

(** Parses null literal *)
val parse_null : val_type Angstrom.t

(** Parses any value literal as expression *)
val parse_value : expr Angstrom.t

(** {1 Identifier parsers} *)

(** Parses identifiers (must not be reserved words) *)
val parse_id : ident Angstrom.t

(** {1 Type parsers} *)

(** Parses type keywords (int, char, bool, string) *)
val parse_type_word : base_type Angstrom.t

(** Parses base types (TypeInt, TypeChar, TypeBool, TypeString) *)
val parse_base_type : _type Angstrom.t

(** Parses variable types (TypeVar of base_type) *)
val parse_var_type : var_type Angstrom.t

(** Parses method return types (including void) *)
val parse_method_type : _type Angstrom.t

(** {1 Modifier parsers} *)

(** Parses zero or more modifiers (public, static, async) *)
val parse_modifiers : modifier list Angstrom.t

(** {1 Expression parsers} *)

(** Main expression parser with operator precedence *)
val parse_ops : expr Angstrom.t

(** Parses assignment expressions *)
val parse_assign : expr Angstrom.t

(** Parses identifier as expression *)
val parse_id_expr : expr Angstrom.t

(** [parse_call_expr arg] parses function calls with given argument parser *)
val parse_call_expr : expr Angstrom.t -> expr Angstrom.t

(** {1 Statement parsers} *)

(** Parses variable declarations *)
val parse_decl : stmt Angstrom.t

(** Parses expression statements *)
val parse_stmt_ops : stmt Angstrom.t

(** [parse_if_else body] parses if-else statements with given body parser *)
val parse_if_else : stmt Angstrom.t -> stmt Angstrom.t

(** [parse_for body] parses for loops with given body parser *)
val parse_for : stmt Angstrom.t -> stmt Angstrom.t

(** [parse_while body] parses while loops with given body parser *)
val parse_while : stmt Angstrom.t -> stmt Angstrom.t

(** Parses return statements *)
val parse_return : stmt Angstrom.t

(** Parses break statements *)
val parse_break : stmt Angstrom.t

(** Parses continue statements *)
val parse_continue : stmt Angstrom.t

(** Parses block statements (enclosed in {}) *)
val parse_block : stmt Angstrom.t

(** {1 Class and program parsers} *)

(** Parses variable declarations (type + identifier) *)
val parse_var : var_decl Angstrom.t

(** Parses field signatures (modifiers, type, identifier, optional initializer) *)
val parse_field_sign : (modifier list * var_type * ident * expr option) Angstrom.t

(** Parses method signatures (modifiers, return type, identifier, parameters) *)
val parse_method_sign : (modifier list * _type * ident * params) Angstrom.t

(** Parses complete method definitions *)
val parse_method_member : field Angstrom.t

(** Parses complete field definitions *)
val parse_field_member : field Angstrom.t

(** Parses class members (fields and methods) enclosed in braces *)
val parse_class_members : field list Angstrom.t

(** Parses complete class definitions *)
val parse_class : c_sharp_class Angstrom.t

(** Parses complete programs *)
val parse_prog : program Angstrom.t

(** {1 Utility functions} *)

(** [apply_parser parser str] applies parser to string and returns result *)
val apply_parser : 'a Angstrom.t -> string -> ('a, string) result

(** [parse_option parser str] tries to parse and returns option *)
val parse_option : 'a Angstrom.t -> string -> 'a option

(** {1 Chain combinators} *)

(** Left-associative chaining combinator *)
val chainl1 : 'a Angstrom.t -> ('a -> 'a -> 'a) Angstrom.t -> 'a Angstrom.t

(** Right-associative chaining combinator *)
val chainr1 : 'a Angstrom.t -> ('a -> 'a -> 'a) Angstrom.t -> 'a Angstrom.t
