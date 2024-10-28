[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

(* https://ocaml.org/manual/5.2/patterns.html *)

open! Base
open Angstrom
open LAst

open Common

let pany = char '_' *> return PatAny
let pvar = pvalue_id >>| fun id -> PatVar id
let pconst = pconst >>| fun const -> PatConst const

(** [Cons (hd, tl)] *)
let pconstruct poprnd =
  let* id = pconstruct_id in
  let* arg = opt poprnd in
  return (PatConstruct (id, arg))

(** [a; b; c] *)
let plist ppat =
  let pelements =
    sep_by1 (ws *> char ';') ppat
    >>| List.fold_right
          ~init:(PatConstruct (Id "[]", None))
          ~f:(fun pat acc ->
            PatConstruct (Id "::", Some (PatTuple (pat, acc, []))) )
  in
  char '[' *> pelements <* ws <* opt (char ';') <* ws <* char ']'

let poprnd ppat =
  fix (fun poprnd ->
      ws
      *> choice [pany; pvar; pconst; pconstruct poprnd; plist ppat; parens ppat] )

(* ======= Operators ======= *)

let table =
  let aor _ lhs rhs = PatOr (lhs, rhs) in

  let atuple _ lhs = function
    | PatTuple (fst, snd, tl) ->
        PatTuple (lhs, fst, snd :: tl)
    | rhs ->
        PatTuple (lhs, rhs, [])
  in

  let alist _ lhs rhs =
    PatConstruct (Id "::", Some (PatTuple (lhs, rhs, [])))
  in

  [ Op {pop= string "::"; kind= Infix {assoc= `Right; apply= alist}}
  ; Op {pop= string ","; kind= Infix {assoc= `Right; apply= atuple}}
  ; Op {pop= string "|"; kind= Infix {assoc= `Left; apply= aor}} ]

let ppat = fix (fun ppat -> poperators ~table ~poprnd:(poprnd ppat))
