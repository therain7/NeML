[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base
open LMisc
open PPrint
open Common

module Prec = struct
  type t = Arr | Tuple | Con | Highest [@@deriving enum]
  let parens = parens
end

let pp =
  let open LAst.Ty in
  let open PrecedencePrinter (Prec) in
  let rec p = function
    | Var id ->
        return Prec.Highest (pp_id id)
    | Con (id, []) ->
        return Prec.Highest (pp_id id)
    | Con (id, [arg]) ->
        let op arg = group @@ arg ^^ space ^^ pp_id id in
        rprefix Prec.Con op (p arg)
    | Con (id, args) ->
        let args = List.map args ~f:(runf p) in
        let doc =
          group @@ parens (flow (comma ^^ break 1) args) ^^ space ^^ pp_id id
        in
        return Prec.Con doc
    | Tuple list2 ->
        let op docs = group @@ flow (break 1 ^^ string "* ") docs in
        rinfixn Prec.Tuple op (List.map (List2.to_list list2) ~f:p)
    | Arr (l, r) ->
        rinfixr Prec.Arr (infixr (string "-> ")) (p l) (p r)
  in

  runf p
