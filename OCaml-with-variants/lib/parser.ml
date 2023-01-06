(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* TODO: implement parser here *)
open Angstrom
open Base
open Caml.Format
open Ast

let is_whitespace = function
  | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
  | _ -> false
;;

let whitespace = take_while is_whitespace

let is_digit = function '0'..'9' -> true | _ -> false

let integer = take_while1 is_digit


module FloatNumParser = struct
  let sign =
    peek_char
    >>= function
      | Some '-' -> advance 1 >>| fun () -> "-"
      | Some '+' -> advance 1 >>| fun () -> "+"
      | Some c when (is_digit c) -> return "+"
      | _ -> fail "Sign or digit expected"
  ;;

  let number =
    sign
    >>= fun sign ->
    take_while1 is_digit <* char '.'
    >>= fun whole ->
    take_while1 is_digit 
    >>= fun part ->
    return (sign ^ whole ^ "." ^ part)
end

module SimpleLangParser = struct
  let keywords_list = ["let"; "in"]

  let is_kw = List.fold

  let token_separator = take_while (is_whitespace)

  let as_token p = token_separator *> p <* token_separator


  let is_letter = function
    | 'a'..'z' -> true
    | 'A'..'Z' -> true
    | _ -> false
  ;;

  let is_digit = function
    | '0'..'9' -> true
    | _ -> false
  ;;
    
  let space = take_while is_whitespace
  let space1 = take_while1 is_whitespace
  let token s = space *> string s


    (* CHANGED!!!!!! *)
  module Literals = struct
    let int_token = space *> (take_while1 is_digit) >>= fun res -> return @@ Exp_literal (Int (int_of_string res))

    let float_token = space *> FloatNumParser.number >>= fun res -> return @@ Exp_literal (Float (float_of_string res))

    let string_token  = 
      space *> char '"' *> take_while (fun c -> not (Char.equal c '"')) <* char '"'
      >>= fun res -> return @@ Exp_literal (String ("\"" ^ res ^ "\"")) 
  end

  module BinOperators = struct
    let ar_operators =
      choice [ token "+"; token "-"; token "/"; token "*" ]
  end

  let new_ident =
    space
    *> (take_while1 is_letter)
    >>= fun str -> 
      if String.equal str "in" then fail "Keyword in the wrong place of program" else return str
  ;;

  let int_number = take_while1 is_digit >>= fun s -> return @@ int_of_string s

  let rec link_exps e_list =
    match e_list with
    | e :: []  -> e
    | h :: t -> Exp_seq (h, (link_exps t))
    | _ -> failwith "empty list"
  ;;

  let let_binding_constructor name arg_list body =
    match arg_list with
    | [] -> Exp_letbinding (name, body)
    | _ -> 
      printf "Size: %i" (List.length arg_list);
      List.iter ~f:(printf "\n%s \n") arg_list;
      failwith "error!!!!"
  ;;

  let rec fun_constructor name args body =
    match args with
    | [ a ] -> Exp_fun (name, a, body)
    | h :: t -> Exp_fun (name, h, fun_constructor name t body)
    | _ -> let_binding_constructor name args body
  ;;

  let literals =
    choice
    [
      Literals.float_token
    ; Literals.int_token
    ; Literals.string_token
    ]

  let arithm_parser =
    let c = choice [(new_ident >>= fun res -> return @@ Exp_ident res); literals] in 
    lift3
      (fun arg1 operator arg2 -> Exp_apply (operator, [arg1; arg2]))
      (space *> c <* space)
      (BinOperators.ar_operators)
      (space *> c <* space)
  ;;

  let arg_of_application =
    choice
    [
      (new_ident >>= fun res -> return @@ Exp_ident res)
    ; literals
    ]
  
  let appl =
    lift2 
      (fun id li -> 
        Exp_apply (id, li))
      (new_ident)
      ((many1 (space1 *> arg_of_application)) <* space)
  ;;

  let decl =
    lift3 
      (fun a b c -> let_binding_constructor a b c)
      ((token "let") *> space1 *> new_ident) 
      (many (space1 *> new_ident))
      (space *> token "=" *> space *> (choice [arithm_parser; literals; appl]) <* space <* token "in")
  ;;

  let base =
    choice [ arithm_parser; decl; appl; (new_ident >>= fun res -> return @@ Exp_ident res) ]

  
  let body = 
    many 
      (base <* char '\n' <|> base <* space1 <|> base) 
      >>= fun res -> return @@ link_exps res

  let hl_fun_decl =
    lift3
      (fun a b c -> fun_constructor a b c)
      ((token "let") *> space1 *> new_ident) 
      (many1 (space1 *> new_ident))
      ((space *> token "=" *> space *> body <* space <* string ";;" <* space))
  ;;

  let p = choice [arithm_parser; hl_fun_decl; appl]

  let p1 = appl 
end

module Priner = struct
  let print_let = function
    | (name, Int i) -> printf "Name: %s; Val: %i\n" name i
    | (name, Float i) -> printf "Name: %s; Val: %f\n" name i
    | (name, String i) -> printf "Name: %s; Val: %s\n" name i
  ;;

  let print_literal = function
    | Int i -> printf "(Int: %i)" i
    | Float f -> printf "(Float: %f)" f
    | String s -> printf "(String: %s)" s
  ;;

  let rec print_ast = function
    | Exp_letbinding (id, value) -> 
      printf "(LetB: Name=%s value=" id;
      print_ast value;
      printf ")"
    | Exp_literal l -> print_literal l
    | Exp_ident i -> printf "(Ident: %s)" i
    | Exp_fun (name, arg, e) ->
      printf "(Fun: name=%s arg=%s" name arg;
      print_ast e;
      printf ")"
    | Exp_seq (e1, e2) -> 
      printf "Seq (";
      print_ast e1;
      print_ast e2;
      printf ")";
    | Exp_apply (name, arg_list) -> printf "(Apply: name=%s Args:" name; List.iter ~f:(print_ast) arg_list; printf ")"
    | _ -> printf "Unrecognised Ast Node"
  ;;
end

let parse_exp code = 
  let result =
    Angstrom.parse_string 
      (SimpleLangParser.p) 
      ~consume:Angstrom.Consume.All 
      code
  in
  result

let print_result = function
  | Result.Ok res -> Priner.print_ast res
  | Result.Error s -> printf "SOMETHING WENT WRONG: %s\n" s 
;;

let () =
  parse_exp "let f q w = let res = w + q in res;;" |> print_result;
  printf "\n";
  parse_exp "1 + 2" |> print_result;
  printf "\n";
  parse_exp 
    {|
      let func a b =
        let r = a + b in
        let d = r * a in
        let k = 30 in
        d - k
      ;;
    |} |> print_result
;;

let should_equal e1 e2 =
  match e2 with
  | Result.Ok e when e = e1 -> true
  | _ -> false
;;

let p2 = parse_exp "1 + 2"
let%test _ =
  match  p2 with
  | Result.Ok (Exp_apply ("+", [Exp_literal (Int 1); Exp_literal (Int 2)])) -> true
  | _ -> false
  
(*let%test _ =
  parse_exp "let f q w = let res = w + q in res;;" = Exp_fun 

*)