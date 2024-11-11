[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base
open LMisc

module Const = struct
  type t =
    | Int of int  (** Integer such as [25] *)
    | Char of char  (** Character such as ['c'] *)
    | String of string
        (** Constant string such as ["constant"] or [{|other constant|}] *)
end

module Ty = struct
  type t =
    | Var of Id.t  (** A type variable such as ['a] *)
    | Arr of t * t  (** [T1 -> T2] *)
    | Tuple of t List2.t  (** [T1 * ... * Tn] *)
    | Con of Id.t * t list
        (** [Con(tconstr, l)] represents:
          - [tconstr]               when [l=[]]
          - [T tconstr]             when [l=[T]]
          - [(T1, ..., Tn) tconstr] when [l=[T1, ..., Tn]]
        *)
end

module Pat = struct
  type t =
    | Any  (** The pattern [_] *)
    | Var of Id.t  (** A variable pattern such as [x] *)
    | Const of Const.t  (** Patterns such as [1], ['a'], ["hello"], [1.5] *)
    | Tuple of t List2.t  (** [(P1, ..., Pn)] *)
    | Or of t * t  (** [P1 | P2] *)
    | Construct of Id.t * t option
        (** [Construct(C, arg)] represents:
          - [C]   when [arg] is [None]
          - [C P] when [arg] is [Some P]
        *)
    | Constraint of t * Ty.t  (** [(P : T)] *)
end

module Expr = struct
  type rec_flag = Rec | Nonrec

  type value_binding = {pat: Pat.t; expr: t}

  (** Pattern matching case *)
  and case = {left: Pat.t; right: t}

  and t =
    | Id of Id.t  (** Identifiers such as [x], [fact] *)
    | Const of Const.t
        (** Expression constant such as [1], ['a'], ["hello"], [1.5] *)
    | Let of rec_flag * value_binding List1.t * t
        (** [Let(flag, [(P1,E1) ; ... ; (Pn,En)], E)] represents:
          - [let P1 = E1 and ... and Pn = EN in E]     when [flag] is [Nonrec]
          - [let rec P1 = E1 and ... and Pn = EN in E] when [flag] is [Rec]
        *)
    | Fun of Pat.t List1.t * t  (** [fun P1 ... Pn -> E] *)
    | Function of case List1.t  (** [function C1 | ... | Cn] *)
    | Apply of t * t  (** [E1 E2] *)
    | Match of t * case List1.t  (** [match E with P1 -> E1 | ... | Pn -> En] *)
    | Tuple of t List2.t  (** [(E1, ..., En)] *)
    | Construct of Id.t * t option
        (** [Construct(C, exp)] represents:
          - [C]               when [exp] is [None]
          - [C E]             when [exp] is [Some E]
          - [C (E1, ..., En)] when [exp] is [Some (Tuple[E1,...,En])]
        *)
    | If of t * t * t option  (** [if E1 then E2 else E3] *)
    | Seq of t List2.t  (** [E1; E2] *)
    | Constraint of t * Ty.t  (** [(E : T)] *)
end

module StrItem = struct
  (** Constructor declaration. E.g. [A of string] *)
  type construct_decl = {id: Id.t; arg: Ty.t option}

  (** Variant type declaration *)
  type type_decl = {id: Id.t; params: Id.t list; variants: construct_decl list}

  type t =
    | Eval of Expr.t  (** [E] *)
    | Type of type_decl  (** [type ('a, 'b) ab = A of T1 | B of T2 ...] *)
    | Let of Expr.rec_flag * Expr.value_binding List1.t
        (** [Let(flag, [(P1, E1) ; ... ; (Pn, En)])] represents:
          - [let P1 = E1 and ... and Pn = EN]      when [flag] is [Nonrec]
          - [let rec P1 = E1 and ... and Pn = EN ] when [flag] is [Rec]
        *)
end

type structure = StrItem.t list
