[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base

(** List containing at least 1 element *)
type 'a list1 = 'a * 'a list [@@deriving show {with_path= false}]

(** List containing at least 2 elements *)
type 'a list2 = 'a * 'a * 'a list [@@deriving show {with_path= false}]

(** Identifiers **)
type ident = Id of string [@@deriving show {with_path= false}]

type constant =
  | ConstInt of int  (** Integer such as [25] *)
  | ConstChar of char  (** Character such as ['c'] *)
  | ConstString of string
      (** Constant string such as ["constant"] or [{|other constant|}] *)
[@@deriving show {with_path= false}]

(* ======= Types ======= *)
type ty =
  | TyVar of ident  (** A type variable such as ['a] *)
  | TyArr of ty * ty  (** [T1 -> T2] *)
  | TyTuple of ty list2  (** [T1 * ... * Tn] *)
  | TyCon of ident * ty list
      (** [TyCon(tconstr, l)] represents:
          - [tconstr]               when [l=[]]
          - [T tconstr]             when [l=[T]]
          - [(T1, ..., Tn) tconstr] when [l=[T1, ..., Tn]]
        *)
[@@deriving show {with_path= false}]

(* ======= Patterns ======= *)
type pattern =
  | PatAny  (** The pattern [_] *)
  | PatVar of ident  (** A variable pattern such as [x] *)
  | PatConst of constant  (** Patterns such as [1], ['a'], ["hello"], [1.5] *)
  | PatTuple of pattern list2  (** [(P1, ..., Pn)] *)
  | PatOr of pattern * pattern  (** [P1 | P2] *)
  | PatConstruct of ident * pattern option
      (** [PatConstruct(C, arg)] represents:
          - [C]   when [arg] is [None]
          - [C P] when [arg] is [Some P]
        *)
  | PatConstraint of pattern * ty  (** [(P : T)] *)
[@@deriving show {with_path= false}]

(* ======= Expressions ======= *)
type rec_flag =
  | Recursive  (** Recursive value binding *)
  | Nonrecursive  (** Nonrecursive value binding *)
[@@deriving show {with_path= false}]

type value_binding = {pat: pattern; expr: expression}
[@@deriving show {with_path= false}]

(** Pattern matching case *)
and case = {left: pattern; right: expression}
[@@deriving show {with_path= false}]

and expression =
  | ExpIdent of ident  (** Identifiers such as [x], [fact] *)
  | ExpConst of constant
      (** Expression constant such as [1], ['a'], ["hello"], [1.5] *)
  | ExpLet of rec_flag * value_binding list1 * expression
      (** [ExpLet(flag, [(P1,E1) ; ... ; (Pn,En)], E)] represents:
          - [let P1 = E1 and ... and Pn = EN in E]     when [flag] is [Nonrecursive]
          - [let rec P1 = E1 and ... and Pn = EN in E] when [flag] is [Recursive]
        *)
  | ExpFun of pattern list1 * expression  (** [fun P1 ... Pn -> E] *)
  | ExpFunction of case list1  (** [function C1 | ... | Cn] *)
  | ExpApply of expression * expression  (** [E1 E2] *)
  | ExpMatch of expression * case list1
      (** [match E with P1 -> E1 | ... | Pn -> En] *)
  | ExpTuple of expression list2  (** [(E1, ..., En)] *)
  | ExpConstruct of ident * expression option
      (** [ExpConstruct(C, exp)] represents:
          - [C]               when [exp] is [None]
          - [C E]             when [exp] is [Some E]
          - [C (E1, ..., En)] when [exp] is [Some (ExpTuple[E1,...,En])]
        *)
  | ExpIfThenElse of expression * expression * expression option
      (** [if E1 then E2 else E3] *)
  | ExpSeq of expression * expression  (** [E1; E2] *)
  | ExpConstraint of expression * ty  (** [(E : T)] *)
[@@deriving show {with_path= false}]

(* ======= Module structure ======= *)

(** Constructor declaration. E.g. [A of string] *)
type constructor_decl = {id: ident; arg: ty option}
[@@deriving show {with_path= false}]

(** Variant type declaration *)
type type_decl = {id: ident; params: ident list; variants: constructor_decl list}
[@@deriving show {with_path= false}]

type structure_item =
  | StrEval of expression  (** [E] *)
  | StrType of type_decl  (** [type ('a, 'b) ab = A of T1 | B of T2 ...] *)
  | StrLet of rec_flag * value_binding list1
      (** [StrLet(flag, [(P1, E1) ; ... ; (Pn, En)])] represents:
          - [let P1 = E1 and ... and Pn = EN]      when [flag] is [Nonrecursive]
          - [let rec P1 = E1 and ... and Pn = EN ] when [flag] is [Recursive]
        *)
[@@deriving show {with_path= false}]

type structure = structure_item list [@@deriving show {with_path= false}]
