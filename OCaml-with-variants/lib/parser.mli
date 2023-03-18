open Ast

val parse_exp : ident -> (top_level_expressions, ident) result
val parse_several_declarations : ident -> (top_level_expressions list, ident) result
