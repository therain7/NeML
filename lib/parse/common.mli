[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base
open Angstrom
open LAst

(* ======= Utils ======= *)
val unit : unit t
val ws : unit t
val ws1 : unit t
val ident : string -> ident t

val parens : 'a t -> 'a t
val opt : 'a t -> 'a option t

(* ======= Identifiers & Constants ======= *)
val pvalue_id : ident t
val pconstr_id : ident t
val pconst : constant t

(* ======= Operators ======= *)
type ('op, 'oprnd) op_kind =
  | Prefix of {apply: 'op -> 'oprnd -> 'oprnd}
  | Infix of {assoc: [`Left | `Right]; apply: 'op -> 'oprnd -> 'oprnd -> 'oprnd}

type 'oprnd op_parse =
  | Op :
      { pop: 'op t  (** Operator symbol parser *)
      ; kind: ('op, 'oprnd) op_kind  (** Kind of an operator *) }
      -> 'oprnd op_parse

(**
  Order in a list sets operators' priority.
  Last operator in a table has the highest priority
*)
type 'oprnd op_parse_table = 'oprnd op_parse list

val poperators : table:'oprnd op_parse_table -> poprnd:'oprnd t -> 'oprnd t
