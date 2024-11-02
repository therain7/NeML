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
open Ty

let pident = pvalue_id >>| fun id -> ExpIdent id
let pconst = pconst >>| fun const -> ExpConst const
let pconstruct = pconstruct_id >>| fun id -> ExpConstruct (id, None)

(**
  [let P1 = E1 and P2 = E2 and ... in E]
  [let rec ValId1 PArg1 = E1 and P1 = E2 and ... in E]
*)
let plet pexpr =
  let* rec_flag, bindings = plet pexpr ppat pty in
  let* expr = spaced (string "in") *> pexpr in
  return (ExpLet (rec_flag, bindings, expr))

(** [fun P1 ... Pn -> E] *)
let pfun pexpr =
  let* args = string "fun" *> ws1 *> sep_by1 ws1 ppat >>| list1_exn in
  let* expr = ws *> string "->" *> pexpr in
  return (ExpFun (args, expr))

let pcases_ pexpr =
  let pcase =
    let* left = ppat in
    let* right = ws *> string "->" *> pexpr in
    return {left; right}
  in
  let pipe (* optional | *) = ws <* char '|' <|> ws1 in
  pipe *> sep_by1 (ws *> char '|') pcase >>| list1_exn

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
  let list hd tl = ExpConstruct (Id "::", Some (ExpTuple (hd, tl, []))) in

  let to_construct = function
    | ExpSeq (e1, e2, tl) ->
        let tl = List.fold_right tl ~init:nil ~f:list in
        list e1 (list e2 tl)
    | e ->
        list e nil
  in
  char '[' *> (pexpr >>| to_construct) <* ws <* opt (char ';') <* ws <* char ']'

(** [if a then b else c] *)
let pif pexpr =
  let* if' = string "if" *> ws1 *> pexpr in
  let* then' = spaced (string "then") *> pexpr in
  let* else' =
    opt (ws1 *> string "else")
    >>= function Some _ -> ws1 *> pexpr >>| Option.some | None -> return None
  in
  return (ExpIf (if', then', else'))

(**
  [ (expr) ]
  [ (expr: ty) ]
*)
let pparens pexpr =
  let p =
    let* expr = pexpr in
    opt (ws *> char ':')
    >>= function
    | None -> return expr | Some _ -> pty >>| fun ty -> ExpConstraint (expr, ty)
  in
  parens p

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
       ; pif pexpr
       ; pparens pexpr ]

(* ======= Operators ======= *)

let table =
  let pprefix1 =
    ws *> pprefix_id >>| fun id rhs -> ExpApply (ExpIdent id, rhs)
  in

  let papply =
    ws1
    >>| fun _ lhs rhs ->
    match lhs with
    | ExpConstruct (id, None) ->
        (* constructor application *)
        ExpConstruct (id, Some rhs)
    | _ ->
        (* function application *)
        ExpApply (lhs, rhs)
  in

  let pprefix2 =
    ws *> choice [string "-" *> return (Id "~-"); string "+" *> return (Id "~+")]
    >>| fun id rhs -> ExpApply (ExpIdent id, rhs)
  in

  let ainfix id lhs rhs = ExpApply (ExpApply (ExpIdent id, lhs), rhs) in

  let pinfix1 = ws *> pinfix_id ~starts:"**" () >>| ainfix in

  let pinfix2 =
    ws
    *> choice
         [ pinfix_id ~starts:"*" ()
         ; pinfix_id ~starts:"/" ()
         ; pinfix_id ~starts:"%" () ]
    >>| ainfix
  in

  let pinfix3 =
    ws *> choice [pinfix_id ~starts:"+" (); pinfix_id ~starts:"-" ()] >>| ainfix
  in

  let plist =
    ws *> string "::"
    >>| fun _ lhs rhs -> ExpConstruct (Id "::", Some (ExpTuple (lhs, rhs, [])))
  in

  let pinfix4 =
    ws *> choice [pinfix_id ~starts:"@" (); pinfix_id ~starts:"^" ()] >>| ainfix
  in

  let pinfix5 =
    ws
    *> choice
         [ pinfix_id ~starts:"=" ()
         ; pinfix_id ~starts:"<" ()
         ; pinfix_id ~starts:">" ()
         ; pinfix_id ~starts:"|" ()
         ; pinfix_id ~starts:"&" ()
         ; pinfix_id ~starts:"$" ()
         ; ident "!=" ]
    >>| ainfix
  in

  let pinfix6 = ws *> ident "&&" >>| ainfix in
  let pinfix7 = ws *> ident "||" >>| ainfix in

  let ptuple = ws *> string "," >>| fun _ list2 -> ExpTuple list2 in
  let pseq = ws *> string ";" >>| fun _ list2 -> ExpSeq list2 in

  [ [Prefix pprefix1]
  ; [InfixL papply]
  ; [Prefix pprefix2]
  ; [InfixR pinfix1]
  ; [InfixL pinfix2]
  ; [InfixL pinfix3]
  ; [InfixR plist]
  ; [InfixR pinfix4]
  ; [InfixL pinfix5]
  ; [InfixR pinfix6]
  ; [InfixR pinfix7]
  ; [InfixN ptuple]
  ; [InfixN pseq] ]

let pexpr = fix (fun pexpr -> poperators ~table ~poprnd:(poprnd pexpr))
