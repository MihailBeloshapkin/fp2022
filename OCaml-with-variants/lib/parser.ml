(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* TODO: implement parser here *)
open Angstrom
open Caml.Format
open Ast

let is_whitespace = function
  | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
  | _ -> false
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let integer = take_while1 is_digit

module FloatNumParser = struct
  let sign =
    peek_char
    >>= function
    | Some '-' -> advance 1 >>| fun () -> "-"
    | Some '+' -> advance 1 >>| fun () -> "+"
    | Some c when is_digit c -> return "+"
    | _ -> fail "Sign or digit expected"
  ;;

  let number =
    sign
    >>= fun sign ->
    take_while1 is_digit
    <* char '.'
    >>= fun whole ->
    take_while1 is_digit >>= fun part -> return (sign ^ whole ^ "." ^ part)
  ;;
end

module OCamlParser = struct
  open Base

  let keywords_list = [ "let"; "in"; "rec"; "if"; "then"; "else"; "match"; "with" ]
  let is_keyword id = List.exists ~f:(fun s -> String.equal s id) keywords_list
  let token_separator = take_while is_whitespace

  let is_letter = function
    | 'a' .. 'z' -> true
    | 'A' .. 'Z' -> true
    | _ -> false
  ;;

  let is_digit = function
    | '0' .. '9' -> true
    | _ -> false
  ;;

  let space = take_while is_whitespace
  let space1 = take_while1 is_whitespace
  let token s = space *> string s

  (* CHANGED!!!!!! *)
  module Literals = struct
    let int_token =
      space *> take_while1 is_digit
      >>= fun res -> return @@ Exp_literal (Int (int_of_string res))
    ;;

    let float_token =
      space *> FloatNumParser.number
      >>= fun res -> return @@ Exp_literal (Float (float_of_string res))
    ;;

    let string_token =
      space *> char '"' *> take_while (fun c -> not (Char.equal c '"'))
      <* char '"'
      >>= fun res -> return @@ Exp_literal (String res)
    ;;

    let true_token = token "true" >>= fun _ -> return @@ Exp_literal (Bool true)
    let false_token = token "false" >>= fun _ -> return @@ Exp_literal (Bool false)
  end

  module BinOperators = struct
    let ar_operators =
      choice
        [ token "+."
        ; token "-."
        ; token "*."
        ; token "/."
        ; token "+"
        ; token "-"
        ; token "/"
        ; token "*"
        ]
    ;;

    let log_operators = choice [ token "&&"; token "||" ]
    let compare_operators = choice [ token ">"; token "<"; token "=" ]
    let binops = choice [ ar_operators; log_operators; compare_operators ]
  end

  let new_ident =
    space *> take_while1 is_letter
    >>= fun str ->
    if is_keyword str then fail "Keyword in the wrong place of program" else return str
  ;;

  let int_number = take_while1 is_digit >>= fun s -> return @@ int_of_string s

  let rec link_exps e_list =
    match e_list with
    | Exp_letbinding (name, body, _) :: t -> Exp_letbinding (name, body, link_exps t)
    | e :: [] -> e
    | h :: t -> Exp_seq (h, link_exps t)
    | _ -> failwith "empty list"
  ;;

  let rec fun_constructor args body =
    match args with
    | [ a ] -> Exp_fun (a, body)
    | h :: t -> Exp_fun (h, fun_constructor t body)
    | _ -> failwith "No args!"
  ;;

  let let_binding_constructor name arg_list body next_ex =
    match arg_list with
    | [] -> Exp_letbinding (name, body, next_ex)
    | _ ->
      let func = fun_constructor arg_list body in
      Exp_letbinding (name, func, next_ex)
  ;;

  let binop_constructor op1 operator op2 =
    match operator with
    | "+" -> Exp_binop (AddInt, op1, op2)
    | "-" -> Exp_binop (SubInt, op1, op2)
    | "*" -> Exp_binop (MulInt, op1, op2)
    | "/" -> Exp_binop (DivInt, op1, op2)
    | "+." -> Exp_binop (AddFloat, op1, op2)
    | "-." -> Exp_binop (SubFloat, op1, op2)
    | "*." -> Exp_binop (MulFloat, op1, op2)
    | "/." -> Exp_binop (DivFloat, op1, op2)
    | "&&" -> Exp_binop (And, op1, op2)
    | "||" -> Exp_binop (Or, op1, op2)
    | "<" -> Exp_binop (Leq, op1, op2)
    | ">" -> Exp_binop (Geq, op1, op2)
    | "=" -> Exp_binop (Eq, op1, op2)
    | _ -> failwith "Parsing error"
  ;;

  let ident_parser = new_ident >>= fun res -> return @@ Exp_ident res
  let unit_parser = token "()" >>= fun _ -> return @@ Exp_unit
  let if_then_else_constructor cond b1 b2 = Exp_ifthenelse (cond, b1, b2)

  let literal_parser =
    choice
      [ Literals.float_token
      ; Literals.int_token
      ; Literals.string_token
      ; Literals.true_token
      ; Literals.false_token
      ]
  ;;

  let arg_of_application = choice [ ident_parser; literal_parser ]

  let appl_parser =
    lift2
      (fun id li -> Exp_apply (id, li))
      new_ident
      (many1 (space1 *> arg_of_application) <* space)
  ;;

  let binop_parser =
    let open BinOperators in
    let c =
      choice
        [ appl_parser
        ; (new_ident >>= fun res -> return @@ Exp_ident res)
        ; literal_parser
        ]
    in
    lift3
      binop_constructor
      (space *> c <* space)
      (choice [ ar_operators; log_operators; compare_operators ])
      (space *> c <* space)
  ;;

  (*
  let letbinding_parser =
    lift3
      (fun name args body -> let_binding_constructor name args body None)
      (token "let" *> space1 *> new_ident)
      (many (space1 *> new_ident))
      (space
       *> token "="
       *> space
       *> choice [ binop_parser; literal_parser; appl_parser; unit_parser ]
      <* space
      <* token "in")
  ;;
*)
  let ifthenelse_parser =
    let cond_variants = [ appl_parser; binop_parser; ident_parser; literal_parser ] in
    let branch_variants =
      [ appl_parser
      ; binop_parser (*    ; letbinding_parser *)
      ; (new_ident >>= fun res -> return @@ Exp_ident res)
      ; literal_parser
      ]
    in
    lift3
      (fun cond fbranch sbranch -> if_then_else_constructor cond fbranch sbranch)
      (token "if" *> choice cond_variants <* token "then")
      (choice branch_variants)
      (token "else" *> choice branch_variants)
  ;;

  let base =
    choice
      [ ifthenelse_parser
      ; binop_parser
      ; literal_parser (*    ; letbinding_parser *)
      ; appl_parser
      ; ident_parser
      ]
  ;;

  let fun_body =
    many (base <* char '\n' <|> base <* space1 <|> base)
    >>= fun res -> return @@ link_exps res
  ;;

  type dispatch =
    { e : dispatch -> exps t
    ; d : dispatch -> exps t
    }

  let type_d =
    let letbinding_parser d =
      fix
      @@ fun _ ->
      lift4
        (fun name args body next -> let_binding_constructor name args body next)
        (token "let" *> space1 *> new_ident)
        (many (space1 *> new_ident))
        (space *> token "=" *> space *> d.e d <* space <* token "in")
        (d.e d)
      <?> "letb"
    in
    let app_parser d =
      let app_arg_p = space1 *> (ident_parser <|> literal_parser) in
      fix
      @@ fun _ ->
      choice
        [ char '(' *> d.e d <* space <* char ')'
        ; (new_ident
          >>= fun n ->
          many1 (app_arg_p <|> (space1 *> char '(' *> d.e d <* space <* char ')'))
          >>= fun args -> return @@ Exp_apply (n, args))
        ]
      <?> "app_parser"
    in
    let binop_parser d =
      fix
      @@ fun _ ->
      let c =
        choice
          [ char '(' *> d.e d <* char ')'; app_parser d; ident_parser; literal_parser ]
      in
      lift3
        binop_constructor
        (space *> c <* space)
        BinOperators.binops
        (space *> c <* space)
    in
    let ifthelse_parser d =
      fix
      @@ fun _ ->
      lift3
        (fun cond fbranch sbranch -> if_then_else_constructor cond fbranch sbranch)
        (token "if" *> d.e d <* token "then")
        (d.e d)
        (token "else" *> d.e d)
    in
    let e d =
      letbinding_parser d
      <|> ifthelse_parser d
      <|> binop_parser d
      <|> app_parser d
      <|> literal_parser
      <|> ident_parser
      <?> "general expr parser"
    in
    let d d = app_parser d in
    { e; d }
  ;;

  let e_p = type_d.e type_d
  let d_p = type_d.d type_d

  let hl_fun_decl =
    lift3
      (fun a b c -> Declaration (a, fun_constructor b c))
      (token "let" *> space1 *> new_ident)
      (many1 (space1 *> new_ident))
      (space *> token "=" *> space *> e_p <* space <* string ";;" <* space)
  ;;

  let p =
    choice
      [ hl_fun_decl
      ; (e_p
        >>= fun res ->
        return
        @@ Application res (*; ifthenelse_parser; binop_parser; hl_fun_decl; appl_parser*)
        )
      ]
  ;;
end

module Printer = struct
  open Base

  let print_let = function
    | name, Int i -> printf "Name: %s; Val: %i\n" name i
    | name, Float i -> printf "Name: %s; Val: %f\n" name i
    | name, String i -> printf "Name: %s; Val: %s\n" name i
    | name, Bool i ->
      printf "Name: %s; Val: " name;
      print_bool i
  ;;

  let print_literal = function
    | Int i -> printf "(Int: %i)" i
    | Float f -> printf "(Float: %f)" f
    | String s -> printf "(String: %s)" s
    | Bool i ->
      printf "(Bool:";
      print_bool i;
      printf ")"
  ;;

  let rec print_ast = function
    | Exp_letbinding (id, value, _) ->
      printf "(LetB: Name=%s value=" id;
      print_ast value;
      printf ")"
    | Exp_literal l -> print_literal l
    | Exp_ident i -> printf "(Ident: %s)" i
    | Exp_fun (arg, e) ->
      printf "(Fun: arg=%s" arg;
      print_ast e;
      printf ")"
    | Exp_seq (e1, e2) ->
      printf "Seq (";
      print_ast e1;
      print_ast e2;
      printf ")"
    | Exp_apply (name, arg_list) ->
      printf "(Apply: name=%s Args:" name;
      List.iter ~f:print_ast arg_list;
      printf ")"
    | Exp_binop (b, l, r) ->
      printf "Binop(";
      print_ast l;
      print_ast r;
      printf ")"
    | Exp_ifthenelse (c, b1, b2) ->
      printf "IfThenElse(";
      print_ast c;
      printf ",";
      print_ast b1;
      printf ",";
      print_ast b2;
      printf ")"
    | _ -> printf "Unrecognised Ast Node"
  ;;
end

let parse_exp code =
  let result = Angstrom.parse_string OCamlParser.p ~consume:Angstrom.Consume.All code in
  result
;;

let print_result = function
  | Result.Ok res -> Printer.print_ast res
  | Result.Error s -> printf "SOMETHING WENT WRONG: %s\n" s
;;

let p2 = parse_exp "if a then b + k else c"

(*
let%test _ =
  match p2 with
  | Result.Error m ->
    printf "Error: %s" m;
    false
  | Result.Ok r ->
    Printer.print_ast r;
    true
;;

*)

let%test _ =
  p2
  = Result.Ok
      (Application
         (Exp_ifthenelse
            ( Exp_ident "a"
            , Exp_binop (AddInt, Exp_ident "b", Exp_ident "k")
            , Exp_ident "c" )))
;;

let p2 = parse_exp "a = 1"
let p2 = parse_exp "let f x = if a = 1 then 1 else n * (f (n + 1));;"

let%test _ =
  match p2 with
  | Result.Error m ->
    printf "Error: %s" m;
    false
  | Result.Ok (Declaration (name, r)) ->
    printf "Name: %s" name;
    Printer.print_ast r;
    true
  | Result.Ok (Application r) ->
    Printer.print_ast r;
    true
;;

let p2 = parse_exp "4 + 2"

let%test _ =
  p2
  = Result.Ok (Application (Exp_binop (AddInt, Exp_literal (Int 4), Exp_literal (Int 2))))
;;

let p2 = parse_exp "abc - asdf "

let%test _ =
  p2 = Result.Ok (Application (Exp_binop (SubInt, Exp_ident "abc", Exp_ident "asdf")))
;;

let p2 = parse_exp "(f a) + (g b)"

let%test _ =
  p2
  = Result.Ok
      (Application
         (Exp_binop
            ( AddInt
            , Exp_apply ("f", [ Exp_ident "a" ])
            , Exp_apply ("g", [ Exp_ident "b" ]) )))
;;

let p2 = parse_exp "a + 2"

let%test _ =
  p2 = Result.Ok (Application (Exp_binop (AddInt, Exp_ident "a", Exp_literal (Int 2))))
;;

let p2 = parse_exp "1.5 +. 2.3"

let%test _ =
  p2
  = Result.Ok
      (Application
         (Exp_binop (AddFloat, Exp_literal (Float 1.5), Exp_literal (Float 2.3))))
;;

let p2 = parse_exp "a < b"

let%test _ = p2 = Result.Ok (Application (Exp_binop (Leq, Exp_ident "a", Exp_ident "b")))

let p2 = parse_exp "a > 1.0"

let%test _ =
  p2 = Result.Ok (Application (Exp_binop (Geq, Exp_ident "a", Exp_literal (Float 1.0))))
;;

let p2 = parse_exp "a = incr 30"

(*
let%test _ =
  match p2 with
  | Result.Error m ->
    printf "Error: %s" m;
    false
  | Result.Ok r ->
    Printer.print_ast r;
    true
;;
*)
let%test _ =
  p2
  = Result.Ok
      (Application
         (Exp_binop (Eq, Exp_ident "a", Exp_apply ("incr", [ Exp_literal (Int 30) ]))))
;;

let p2 = parse_exp "let f x = if x = 1 then 30 else x ;;"

let%test _ =
  p2
  = Result.Ok
      (Declaration
         ( "f"
         , Exp_fun
             ( "x"
             , Exp_ifthenelse
                 ( Exp_binop (Eq, Exp_ident "x", Exp_literal (Int 1))
                 , Exp_literal (Int 30)
                 , Exp_ident "x" ) ) ))
;;

let p2 = parse_exp "let f x = let a = 10 in a;;"

let%test _ =
  match p2 with
  | Result.Error m ->
    printf "Error: %s" m;
    false
  | Result.Ok (Declaration (name, r)) ->
    printf "Name: %s" name;
    Printer.print_ast r;
    true
  | Result.Ok (Application r) ->
    Printer.print_ast r;
    true
;;

(*
let p2 = parse_exp "let f q w = let res = 10 in w;;"

let%test _ =
  match p2 with
  | Result.Error m ->
    printf "Error: %s" m;
    false
  | Result.Ok r ->
    Printer.print_ast r;
    true
;;

let%test _ =
  p2
  = Result.Ok
      (Exp_letbinding
         ( "f"
         , Exp_fun
             ( "q"
             , Exp_fun
                 ( "w"
                 , Exp_letbinding
                     ( "res"
                     , Exp_binop (AddInt, Exp_ident "w", Exp_ident "q")
                     , Some (Exp_ident "res") ) ) )
         , None ))
;;


*)

(*
let p2 = parse_exp "let fact n = if a < 2 then s else 1 * fact n;;"

let%test _ =
  match p2 with
  | Result.Error m ->
    printf "Error: %s" m;
    false
  | Result.Ok r ->
    Printer.print_ast r;
    true
;;

(*
let%test _ = p2 = Result.Ok (Exp_ifthenelse (Exp_binop (Eq, Exp_ident "a", Exp_literal (Int 1)), Exp_ident "b", Exp_ident "c"))
*)
let p2 = parse_exp "f a + g b"

let%test _ =
  p2
  = Result.Ok
      (Exp_binop
         (AddInt, Exp_apply ("f", [ Exp_ident "a" ]), Exp_apply ("g", [ Exp_ident "b" ])))
;;


let p2 = parse_exp "a + 2"

let%test _ = p2 = Result.Ok (Exp_binop (AddInt, Exp_ident "a", Exp_literal (Int 2)))


let p2 = parse_exp "1.5 +. 2.3"

let%test _ =
  p2 = Result.Ok (Exp_binop (AddFloat, Exp_literal (Float 1.5), Exp_literal (Float 2.3)))
;;

let p2 = parse_exp "a < b"

let%test _ = p2 = Result.Ok (Exp_binop (Leq, Exp_ident "a", Exp_ident "b"))

let p2 = parse_exp "a > 1.0"

let%test _ = p2 = Result.Ok (Exp_binop (Geq, Exp_ident "a", Exp_literal (Float 1.0)))


let p2 = parse_exp "let incr x = x + 1;;"

let%test _ =
  p2
  = Result.Ok
      (Exp_letbinding
         ( "incr"
         , Exp_fun ("x", Exp_binop (AddInt, Exp_ident "x", Exp_literal (Int 1)))
         , None ))
;;

let p2 = parse_exp "let c s = concat s \"ml\";;"

let%test _ =
  p2
  = Result.Ok
      (Exp_letbinding
         ( "c"
         , Exp_fun
             ("s", Exp_apply ("concat", [ Exp_ident "s"; Exp_literal (String "ml") ]))
         , None ))
;;

let p2 = parse_exp "c \"asdf\""

let%test _ = p2 = Result.Ok (Exp_apply ("c", [ Exp_literal (String "asdf") ]))

let p2 = parse_exp "let f q w = let res = w + q in res;;"

let%test _ =
  p2
  = Result.Ok
      (Exp_letbinding
         ( "f"
         , Exp_fun
             ( "q"
             , Exp_fun
                 ( "w"
                 , Exp_letbinding
                     ( "res"
                     , Exp_binop (AddInt, Exp_ident "w", Exp_ident "q")
                     , Some (Exp_ident "res") ) ) )
         , None ))
;;
(*
let p2 = parse_exp "let f x = if x = 1 then 30 else x ;;"

let%test _ =
  p2
  = Result.Ok
      (Exp_letbinding
         ( "f"
         , Exp_fun
             ( "x"
             , Exp_ifthenelse
                 ( Exp_binop (Eq, Exp_ident "x", Exp_literal (Int 1))
                 , Exp_literal (Int 30)
                 , Exp_ident "x" ) )
         , None ))
;;
*)
*)
