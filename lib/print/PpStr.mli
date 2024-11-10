[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base

val pp_stritem : LAst.StrItem.t -> PPrint.document
val pp_structure : LAst.structure -> PPrint.document
