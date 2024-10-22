[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

(* https://ocaml.org/manual/5.2/modules.html#s:module-expr *)

open! Base
open Angstrom
open LAst

open Common

let plet =
  let* rec_flag, bindings = plet Expr.pexpr Pat.ppat in
  opt @@ spaced (string "in")
  >>= function
  | None ->
      return (StrLet (rec_flag, bindings))
  | Some _ ->
      Expr.pexpr >>| fun expr -> StrEval (ExpLet (rec_flag, bindings, expr))

let pty_params_ =
  let pmultiple = parens @@ sep_by1 (ws *> char ',') (ws *> pty_var_id) in
  let psingle = pty_var_id >>| List.return in
  psingle <|> pmultiple <|> return []

let pconstruct_decl_ =
  let* id = pconstruct_id in
  let* arg =
    opt @@ spaced (string "of")
    >>= function None -> return None | Some _ -> Ty.pty >>| Option.some
  in
  return {id; arg}

let pty_decl =
  let* params = string "type" *> ws1 *> pty_params_ in
  let* id = ws *> pty_con_id in
  let* variants =
    ws *> string "=" *> ws
    *> opt (string "|")
    *> sep_by1 (ws *> char '|') (ws *> pconstruct_decl_)
  in
  return (StrType {id; params; variants})

let peval = Expr.pexpr >>| fun expr -> StrEval expr

let pstr_item = ws *> choice [plet; pty_decl; peval]

let pstr =
  let sep = ws *> opt (string ";;") in
  sep_by sep pstr_item <* sep
