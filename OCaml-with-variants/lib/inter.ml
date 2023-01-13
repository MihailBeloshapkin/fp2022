open Base
open Ast

module type Fail_monad = sig
  include Base.Monad.S2

  val fail : 'e -> ('a, 'e) t
  val run : ('a, 'e) t -> ok:('a -> ('b, 'e) t) -> err:('e -> ('b, 'e) t) -> ('b, 'e) t
  val ( let* ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
end

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

  module ContextData = struct
    type t =
      | Int of int
      | Float of float
      | String of string
      | Func of exps
  end

  let get_val_from_ctx ctx name =
    List.find_exn ~f:(fun (n, _) -> String.equal n name) ctx |> snd
  ;;

  let compute_arithm = function
    | _ -> ()
  ;;

  let get_fun_args_and_body f =
    let rec helper exp acc =
      match exp with
      | Exp_fun (name, next) -> helper next (name :: acc)
      | _ as body -> (acc |> List.rev, body)
    in
    helper f []
  ;;

  let rec eval ctx = function
    | Exp_literal (Int i) -> return (ContextData.Int i)
    | Exp_literal (Float f) -> return (ContextData.Float f)
    | Exp_literal (String s) -> return (ContextData.String s)
    | Exp_ident id -> return @@ get_val_from_ctx ctx id
    | Exp_seq (e1, e2) -> return (ContextData.Int 1)
    | Exp_letbinding (id, ex, Some next) ->
      let* _, updated_ctx = eval_let ctx (id, ex) in
      return updated_ctx >>= fun s -> eval s next
    (*| Exp_fun (arg, e) -> 
      
      return*)
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
          let updater_ctx = List.zip_exn arg_names arg_values in
          return @@ eval (List.concat [updater_ctx; ctx]) body
        | _ -> fail "Error: function expected"
      in
      result
    (*| Exp_ident i -> *)
    | _ -> failwith "not impl"

  and eval_args ctx arg_list =
    let arg_values =
      arg_list
      |> List.map ~f:(fun arg ->
           let* result = eval ctx arg in
           return result)
    in
    return arg_values

  and eval_let ctx (id, ex) =
    let* computed_val = eval ctx ex in
    let updated_ctx = (id, computed_val) :: ctx in
    return (computed_val, updated_ctx)
  ;;
end
