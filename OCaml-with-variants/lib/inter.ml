open Ast
open Caml.Format

module I = struct
  include Base.Result

  let ( let* ) x f = x >>= f

  let run x ~ok ~err =
    match x with
    | Ok v -> ok v
    | Error e -> err e
  ;;
end

module Interpreter = struct
  open I
  open Base

  module ContextData = struct
    type t =
      | Int of int
      | Float of float
      | String of string
      | Bool of bool
      | Func of exps
      | Undefined
  end

  let get_val_from_ctx ctx name =
    List.find_exn ~f:(fun (n, _) -> String.equal n name) ctx |> snd
  ;;

  let get_fun_args_and_body f =
    let rec helper exp acc =
      match exp with
      | Exp_fun (name, next) -> helper next (name :: acc)
      | _ as body -> acc |> List.rev, body
    in
    helper f []
  ;;

  let eval_binop op l r =
    let open ContextData in
    let open Caml in
    match op, l, r with
    | AddInt, Int x, Int y -> return (Int (x + y))
    | SubInt, Int x, Int y -> return (Int (x - y))
    | MulInt, Int x, Int y -> return (Int (x * y))
    | DivInt, Int x, Int y -> return (Int (x / y))
    | AddFloat, Float x, Float y -> return (Float (x +. y))
    | SubFloat, Float x, Float y -> return (Float (x -. y))
    | MulFloat, Float x, Float y -> return (Float (x *. y))
    | DivFloat, Float x, Float y -> return (Float (x /. y))
    | EqInt, Int x, Int y -> return (Bool (x = y))
    | LeqInt, Int x, Int y -> return (Bool (x < y))
    | GeqInt, Int x, Int y -> return (Bool (x > y))
    | _ -> fail "Unrecognised operation"
  ;;

  let rec eval ctx = function
    | Exp_literal (Int i) -> return (ContextData.Int i)
    | Exp_literal (Float f) -> return (ContextData.Float f)
    | Exp_literal (String s) -> return (ContextData.String s)
    | Exp_binop (binop, left, right) ->
      let* l_evaluated = eval ctx left in
      let* r_evaluated = eval ctx right in
      let* res = eval_binop binop l_evaluated r_evaluated in
      return res
    | Exp_ident id -> return @@ get_val_from_ctx ctx id
    | Exp_seq (e1, e2) -> return (ContextData.Int 1)
    | Exp_letbinding (_, id, ex, next) ->
      let* _, updated_ctx = eval_let ctx (id, ex) in
      return updated_ctx >>= fun s -> eval s next
    | Exp_apply (id, args) ->
      let func = get_val_from_ctx ctx id in
      let* result =
        match func with
        | ContextData.Func e ->
          let arg_names, body = get_fun_args_and_body e in
          let arg_values =
            args
            |> List.map ~f:(fun arg ->
                 match eval ctx arg with
                 | Ok data -> data
                 | _ -> failwith "cant eval")
          in
          let updated_ctx = List.zip_exn arg_names arg_values in
          return @@ eval (List.concat [ updated_ctx; ctx ]) body
        | _ -> fail "Error: function expected"
      in
      result
    (*| Exp_ident i -> *)
    | Exp_ifthenelse (cond, f_branch, s_branch) ->
      let* cond_eval = eval ctx cond in
      let result =
        match cond_eval with
        | ContextData.Bool true -> eval ctx f_branch
        | ContextData.Bool false -> eval ctx s_branch
        | _ -> fail "oh"
      in
      result
    | Exp_match (e, cases) ->
      let* result = eval ctx e in
      return result
    | _ -> fail "not impl"

  and eval_let ctx (id, ex) =
    let* computed_val = eval ctx ex in
    let updated_ctx = (id, computed_val) :: ctx in
    return (computed_val, updated_ctx)
  ;;
end

let eval = Interpreter.eval

let p =
  Interpreter.eval [] (Exp_binop (AddInt, Exp_literal (Int 30), Exp_literal (Int 30)))
;;

let%test _ = p = Ok (Interpreter.ContextData.Int 60)

let p =
  Interpreter.eval
    []
    (Exp_binop
       ( AddInt
       , Exp_literal (Int 30)
       , Exp_binop (AddInt, Exp_literal (Int 3), Exp_literal (Int 4)) ))
;;

let%test _ = p = Ok (Interpreter.ContextData.Int 37)

let p =
  Interpreter.eval
    [ "x", Interpreter.ContextData.Int 3; "y", Interpreter.ContextData.Int 4 ]
    (Exp_binop (AddInt, Exp_ident "x", Exp_ident "y"))
;;

let%test _ = p = Ok (Interpreter.ContextData.Int 7)

let p =
  Interpreter.eval
    [ ( "f"
      , Interpreter.ContextData.Func
          (Exp_fun ("x", Exp_binop (AddInt, Exp_ident "x", Exp_literal (Int 1)))) )
    ]
    (Exp_apply ("f", [ Exp_literal (Int 30) ]))
;;

let%test _ = p = Ok (Interpreter.ContextData.Int 31)

let p =
  Interpreter.eval
    [ ( "f"
      , Interpreter.ContextData.Func
          (Exp_fun
             ( "x"
             , Exp_letbinding
                 ( NonRec
                 , "v"
                 , Exp_literal (Int 1)
                 , Exp_binop (AddInt, Exp_ident "x", Exp_ident "v") ) )) )
    ]
    (Exp_apply ("f", [ Exp_literal (Int 30) ]))
;;

let%test _ = p = Ok (Interpreter.ContextData.Int 31)

let p =
  (* f x =  let v = 1 in let r = 4 in x * v + r*)
  Interpreter.eval
    [ ( "f"
      , Interpreter.ContextData.Func
          (Exp_fun
             ( "x"
             , Exp_letbinding
                 ( NonRec
                 , "v"
                 , Exp_literal (Int 1)
                 , Exp_letbinding
                     ( NonRec
                     , "r"
                     , Exp_literal (Int 4)
                     , Exp_binop
                         ( AddInt
                         , Exp_binop (MulInt, Exp_ident "x", Exp_ident "v")
                         , Exp_ident "r" ) ) ) )) )
    ]
    (Exp_apply ("f", [ Exp_literal (Int 30) ]))
;;

let%test _ = p = Ok (Interpreter.ContextData.Int 34)

let p =
  Interpreter.eval
    [ ( "incr"
      , Interpreter.ContextData.Func
          (Exp_fun ("x", Exp_binop (AddInt, Exp_ident "x", Exp_literal (Int 1)))) )
    ]
    (Exp_binop (AddInt, Exp_literal (Int 7), Exp_apply ("incr", [ Exp_literal (Int 30) ])))
;;

let%test _ = p = Ok (Interpreter.ContextData.Int 38)

let p =
  Interpreter.eval
    [ ( "func"
      , Interpreter.ContextData.Func
          (Exp_fun
             ( "x"
             , Exp_ifthenelse
                 ( Exp_binop (EqInt, Exp_ident "x", Exp_literal (Int 1))
                 , Exp_literal (Int 30)
                 , Exp_ident "x" ) )) )
    ]
    (Exp_apply ("func", [ Exp_literal (Int 1) ]))
;;

let%test _ = p = Ok (Interpreter.ContextData.Int 30)

let p =
  Interpreter.eval
    [ ( "fact"
      , Interpreter.ContextData.Func
          (Exp_fun
             ( "n"
             , Exp_ifthenelse
                 ( Exp_binop (LeqInt, Exp_ident "n", Exp_literal (Int 2))
                 , Exp_literal (Int 1)
                 , Exp_binop
                     ( MulInt
                     , Exp_ident "n"
                     , Exp_apply
                         ( "fact"
                         , [ Exp_binop (SubInt, Exp_ident "n", Exp_literal (Int 1)) ] ) )
                 ) )) )
    ]
    (Exp_apply ("fact", [ Exp_literal (Int 5) ]))
;;

let%test _ = p = Ok (Interpreter.ContextData.Int 120)
