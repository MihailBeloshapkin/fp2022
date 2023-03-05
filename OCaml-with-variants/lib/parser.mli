(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)
open Ast

val parse_exp : ident -> (top_level_expressions, ident) result
