open Base
open Parser
open Ast
open Inter
open Inter.Interpreter.ContextData

let get_exp_type = function
  | Exp_fun _ as f -> Func f
  | Exp_literal (Int i) -> Int i
  | Exp_literal (Float f) -> Float f
  | _ -> Undefined
;;

let output = function
  | Ok (Interpreter.ContextData.Int i) -> Caml.Format.printf "%i" i

  | _ -> Caml.Format.printf "FAILED"

let rec run input ctx =
  let parse_tree = Parser.parse_exp input in
  match parse_tree with
  | Error _ -> failwith "parsing error"
  | Ok (Declaration (_, name, decl)) -> 
    let updated_ctx = (name, (get_exp_type decl)) :: ctx in
    ()
  | Ok (Application e) ->
    let interpreted = Inter.Interpreter.eval ctx e in
    ()
;;
