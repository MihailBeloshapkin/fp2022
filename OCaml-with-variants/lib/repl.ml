open Base
open Parser
open Ast
open Inter
open Inter.Interpreter.ContextData
open Stdio

let get_exp_type = function
  | Exp_fun _ as f -> Func f
  | Exp_literal (Int i) -> Int i
  | Exp_literal (Float f) -> Float f
  | _ -> Undefined
;;

let output = function
  | Ok (Interpreter.ContextData.Int i) -> Caml.Format.printf "%i" i
  | Ok (Interpreter.ContextData.Float f) -> Caml.Format.printf "%f" f
  | Ok (Interpreter.ContextData.String s) -> Caml.Format.printf "%s" s
  | Ok ((Interpreter.ContextData.Bool b)) -> Caml.Format.printf "%b" b
  | _ -> Caml.Format.printf "FAILED"
;;

let read_next prev =
  let input = read_line () in
  input ^ prev
;;

let rec run ctx =
  let open Caml.Format in
  let input = match Stdio.In_channel.input_line Caml.stdin with Some x -> x | _ -> ""  in
  let parse_tree = Parser.parse_exp input in
  printf "\nInput: %s\n" input;
  match parse_tree with
  | Error _ -> 
    printf "Oh";
    (*run ctx*)
  | Ok (Declaration (_, name, decl)) -> 
    printf "Declared";
    let updated_ctx = (name, (get_exp_type decl)) :: ctx in
    run updated_ctx
  | Ok (Application e) ->
    Caml.Format.printf "\nSuccess\n";
    let interpreted = Inter.Interpreter.eval ctx e in
    let new_ctx = ctx in
    output interpreted;
    run new_ctx
;;

let run0 decl appl =
  let open Caml.Format in
  let decl_ast = Parser.parse_exp decl in
  let appl_ast = Parser.parse_exp appl in
  match decl_ast with
  | Ok (Declaration (_, name, d)) -> 
    let ctx = [(name, (get_exp_type d))] in
    let () =
      match appl_ast with
      | Ok (Application e) ->
        let interpreted = Inter.Interpreter.eval ctx e in
        output interpreted;
      | _ -> ()
    in ()
  | _ -> printf "error"
;;