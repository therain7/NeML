[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

(* https://ocaml.org/manual/5.2/patterns.html *)

open! Base
open Angstrom
open LAst

open Common
open Ty

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

(**
  [ (pat) ]
  [ (pat: ty) ]
*)
let pparens ppat =
  let p =
    let* pat = ppat in
    opt (ws *> char ':')
    >>= function
    | None -> return pat | Some _ -> pty >>| fun ty -> PatConstraint (pat, ty)
  in
  parens p

let poprnd ppat =
  fix (fun poprnd ->
      ws
      *> choice [pany; pvar; pconst; pconstruct poprnd; plist ppat; pparens ppat] )

(* ======= Operators ======= *)

let table =
  let plist =
    ws *> string "::"
    >>| fun _ lhs rhs -> PatConstruct (Id "::", Some (PatTuple (lhs, rhs, [])))
  in
  let ptuple = ws *> string "," >>| fun _ list2 -> PatTuple list2 in
  let por = ws *> string "|" >>| fun _ lhs rhs -> PatOr (lhs, rhs) in

  [[InfixR plist]; [InfixN ptuple]; [InfixL por]]

let ppat = fix (fun ppat -> poperators ~table ~poprnd:(poprnd ppat))
