[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

(* https://ocaml.org/manual/5.2/expr.html *)

open! Base
open Angstrom
open LAst

open Common
open Pat

let pident = pvalue_id >>| fun id -> ExpIdent id
let pconst = pconst >>| fun const -> ExpConst const
let pconstruct = pconstruct_id >>| fun id -> ExpConstruct (id, None)

(**
  [let P1 = E1 and P2 = E2 and ... in E]
  [let rec ValId1 PArg1 = E1 and P1 = E2 and ... in E]
*)
let plet pexpr =
  let* rec_flag, bindings = plet pexpr ppat in
  let* expr = spaced (string "in") *> pexpr in
  return (ExpLet (rec_flag, bindings, expr))

(** [fun P1 ... Pn -> E] *)
let pfun pexpr =
  let* args = string "fun" *> ws1 *> sep_by1 ws1 ppat in
  let* expr = ws *> string "->" *> pexpr in
  return (ExpFun (args, expr))

let pcases_ pexpr =
  let pcase =
    let* left = ppat in
    let* right = ws *> string "->" *> pexpr in
    return {left; right}
  in
  let pipe (* optional | *) = ws <* char '|' <|> ws1 in
  pipe *> sep_by1 (ws *> char '|') pcase

(** [match E0 with P1 -> E1 | ... | Pn -> En] *)
let pmatch pexpr =
  let* expr = string "match" *> ws1 *> pexpr in
  let* cases = ws1 *> string "with" *> pcases_ pexpr in
  return (ExpMatch (expr, cases))

(** [function P1 -> E1 | ... | Pn -> En] *)
let pfunction pexpr =
  string "function" *> pcases_ pexpr >>| fun cases -> ExpFunction cases

(** [a; b; c] *)
let plist pexpr =
  let nil = ExpConstruct (Id "[]", None) in
  let list hd tl = ExpConstruct (Id "::", Some (ExpTuple [hd; tl])) in

  let rec to_construct = function
    | ExpSeq (e1, e2) ->
        list e1 (to_construct e2)
    | e ->
        list e nil
  in
  char '[' *> (pexpr >>| to_construct) <* ws <* opt (char ';') <* ws <* char ']'

let poprnd pexpr =
  ws
  *> choice
       [ pident
       ; pconst
       ; pconstruct
       ; plet pexpr
       ; pfun pexpr
       ; pmatch pexpr
       ; pfunction pexpr
       ; plist pexpr
       ; parens pexpr ]

(* ======= Operators ======= *)

let table pexpr =
  let aseq _ lhs rhs = ExpSeq (lhs, rhs) in

  let pif =
    let* if_ = string "if" *> ws1 *> pexpr in
    spaced (string "then") *> return if_
  in
  let aif if_ then_ = ExpIfThenElse (if_, then_, None) in

  let pif_else =
    let* if_ = string "if" *> ws1 *> pexpr in
    let* then_ = spaced (string "then") *> pexpr in
    spaced (string "else") *> return (if_, then_)
  in
  let aif_else (if_, then_) else_ = ExpIfThenElse (if_, then_, Some else_) in

  let atuple _ lhs = function
    | ExpTuple tl ->
        ExpTuple (lhs :: tl)
    | rhs ->
        ExpTuple [lhs; rhs]
  in

  let ainfix op_id lhs rhs = ExpApply (ExpApply (ExpIdent op_id, lhs), rhs) in

  let alist _ lhs rhs = ExpConstruct (Id "::", Some (ExpTuple [lhs; rhs])) in

  let aprefix_minus _ rhs = ExpApply (ExpIdent (Id "~-"), rhs) in

  let aapply _ lhs rhs =
    match lhs with
    | ExpConstruct (id, None) ->
        (* constructor application *)
        ExpConstruct (id, Some rhs)
    | _ ->
        (* function application *)
        ExpApply (lhs, rhs)
  in

  let aprefix id rhs = ExpApply (ExpIdent id, rhs) in

  [ Op {pop= string ";"; kind= Infix {assoc= `Right; apply= aseq}}
    (* XXX: severe backtracking when else not found *)
  ; Op {pop= pif_else; kind= Prefix {apply= aif_else}}
  ; Op {pop= pif; kind= Prefix {apply= aif}}
  ; Op {pop= string ","; kind= Infix {assoc= `Right; apply= atuple}}
  ; Op {pop= ident "||"; kind= Infix {assoc= `Right; apply= ainfix}}
  ; Op {pop= ident "&&"; kind= Infix {assoc= `Right; apply= ainfix}}
  ; Op
      { pop=
          choice
            [ pinfix_id ~starts:"=" ()
            ; pinfix_id ~starts:"<" ()
            ; pinfix_id ~starts:">" ()
            ; pinfix_id ~starts:"|" ()
            ; pinfix_id ~starts:"&" ()
            ; pinfix_id ~starts:"$" ()
            ; ident "!=" ]
      ; kind= Infix {assoc= `Left; apply= ainfix} }
  ; Op
      { pop= pinfix_id ~starts:"@" () <|> pinfix_id ~starts:"^" ()
      ; kind= Infix {assoc= `Right; apply= ainfix} }
  ; Op {pop= string "::"; kind= Infix {assoc= `Right; apply= alist}}
  ; Op
      { pop= pinfix_id ~starts:"+" () <|> pinfix_id ~starts:"-" ()
      ; kind= Infix {assoc= `Left; apply= ainfix} }
  ; Op
      { pop=
          choice
            [ pinfix_id ~starts:"*" ()
            ; pinfix_id ~starts:"/" ()
            ; pinfix_id ~starts:"%" () ]
      ; kind= Infix {assoc= `Left; apply= ainfix} }
  ; Op
      { pop= pinfix_id ~starts:"**" ()
      ; kind= Infix {assoc= `Right; apply= ainfix} }
  ; Op {pop= string "-"; kind= Prefix {apply= aprefix_minus}}
  ; Op {pop= unit; kind= Infix {assoc= `Left; apply= aapply}}
  ; Op
      { pop= pprefix_id <|> string "+" *> return (Id "~+")
      ; kind= Prefix {apply= aprefix} } ]

let pexpr =
  fix (fun pexpr -> poperators ~table:(table pexpr) ~poprnd:(poprnd pexpr))
