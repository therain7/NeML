[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base
open Angstrom
open LAst

(* ======= Utils ======= *)
val list1_exn : 'a list -> 'a list1

val unit : unit t
val ws : unit t
val ws1 : unit t
val ident : string -> ident t

val parens : 'a t -> 'a t
val spaced : 'a t -> 'a t
val opt : 'a t -> 'a option t

(* ======= Identifiers ======= *)
val pconstruct_id : ident t
val pvalue_id : ident t
val pty_var_id : ident t
val pty_con_id : ident t

val pinfix_id : ?starts:string -> unit -> ident t
val pprefix_id : ident t

(* ===== Constants, value bindings ===== *)
val pconst : constant t
val plet : expression t -> pattern t -> (rec_flag * value_binding list1) t

(* ======= Operators ======= *)
type 'oprnd operator =
  | Prefix of ('oprnd -> 'oprnd) t  (** Prefix operator *)
  | InfixN of ('oprnd list2 -> 'oprnd) t
      (** Non-associative infix operator. Parsed to a list of operands *)
  | InfixL of ('oprnd -> 'oprnd -> 'oprnd) t
      (** Left associative infix operator *)
  | InfixR of ('oprnd -> 'oprnd -> 'oprnd) t
      (** Right associative infix operator *)

val poperators : table:'oprnd operator list list -> poprnd:'oprnd t -> 'oprnd t
(**
  Table is ordered in descending precedence.
  All operators in one list have the same precedence
  (but may have different associativity)
 *)
