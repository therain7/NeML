[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

(* https://ocaml.org/manual/5.2/types.html#s:typexpr *)

open! Base
open Angstrom
open LAst

open Common

(** 'a, 'b *)
let pvar = pty_var_id >>| fun id -> TyVar id

(** ('k, 'v) map *)
let pmulti_args_app pty =
  let* args = parens @@ sep_by1 (ws *> char ',') pty in
  let* id = ws *> pty_con_id in
  return (TyCon (id, args))

let poprnd pty =
  ws
  *> choice
       [ (pty_con_id >>| fun id -> TyCon (id, []))
       ; pvar
       ; pmulti_args_app pty
       ; parens pty ]

(**
  int list
  ('k, 'v) map option option
*)
let papp pty =
  let* arg = poprnd pty in
  let rec pcons acc =
    option acc (ws1 *> pty_con_id >>= fun id -> pcons (TyCon (id, [acc])))
  in
  pcons arg

let table =
  let aarr _ lhs rhs = TyArr (lhs, rhs) in

  let atuple _ lhs = function
    | TyTuple tl ->
        TyTuple (lhs :: tl)
    | rhs ->
        TyTuple [lhs; rhs]
  in

  [ Op {pop= string "*"; kind= Infix {assoc= `Right; apply= atuple}}
  ; Op {pop= string "->"; kind= Infix {assoc= `Right; apply= aarr}} ]

let pty = fix (fun pty -> poperators ~table ~poprnd:(papp pty))
