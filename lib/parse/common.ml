[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base
open Angstrom
open LAst

(* ======= Utils ======= *)

let unit = return ()

let skip_ws = skip_while Char.is_whitespace
let skip_comments = string "(*" *> many_till any_char (string "*)") *> unit

let ws = skip_ws *> sep_by skip_ws skip_comments *> unit
let ws1 = skip Char.is_whitespace *> ws

let ident s = string s >>| fun x -> Id x

let parens p = char '(' *> ws *> p <* ws <* char ')'
let opt p = option None (p >>| Option.some)

(* ======= Identifiers ======= *)
(* https://ocaml.org/manual/5.2/lex.html#sss:lex:identifiers *)

let is_keyword = function
  (* https://ocaml.org/manual/5.2/lex.html#sss:keywords *)
  | "true"
  | "false"
  | "match"
  | "with"
  | "let"
  | "rec"
  | "and"
  | "in"
  | "type"
  | "function"
  | "fun"
  | "if"
  | "then"
  | "else" ->
      true
  | _ ->
      false

(* XXX: operator keywords? *)
(* let is_keyword_op _ = false *)

let pident flag =
  let is_first =
    match flag with
    | `LowerCase -> (
        function 'a' .. 'z' | '_' -> true | _ -> false )
    | `Capitalized -> (
        function 'A' .. 'Z' -> true | _ -> false )
  in

  let* first = satisfy is_first >>| String.of_char in
  let* rest =
    take_while (function
      | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '\'' ->
          true
      | _ ->
          false )
  in
  let id = first ^ rest in
  if is_keyword id then fail "keyword" else return (Id id)

let pvalue_id = pident `LowerCase

let pconstr_id =
  let keywords =
    choice
      [ident "true"; ident "false"; ident "()"; ident "[]"; parens (ident "::")]
  in
  pident `Capitalized <|> keywords

(* ======= Constants ======= *)

let pconst =
  let pint =
    let* num = take_while Char.is_digit in
    match Int.of_string_opt num with
    | None ->
        fail "not an integer"
    | Some x ->
        return (ConstInt x)
  in

  let pchar = char '\'' *> any_char <* char '\'' >>| fun x -> ConstChar x in

  let pstring =
    char '"' *> take_till (Char.( = ) '"')
    <* advance 1
    >>| fun x -> ConstString x
  in

  choice [pint; pchar; pstring]

(* ======= Operators ======= *)

type ('op, 'oprnd) op_kind =
  | Prefix of {apply: 'op -> 'oprnd -> 'oprnd}
  | Infix of {assoc: [`Left | `Right]; apply: 'op -> 'oprnd -> 'oprnd -> 'oprnd}

type 'oprnd op_parse =
  | Op : {pop: 'op t; kind: ('op, 'oprnd) op_kind} -> 'oprnd op_parse

type 'oprnd op_parse_table = 'oprnd op_parse list

let poperators ~(table : 'oprnd op_parse_table) ~(poprnd : 'oprnd t) =
  (* Convert the table to lists of infix/prefix parsers
     with explicit priorities assigned *)
  let _, prefixs, infixs =
    List.fold table ~init:(0, [], []) ~f:(fun (prio, prefixs, infixs) (Op op) ->
        match op.kind with
        | Prefix {apply} ->
            let pop = op.pop >>| fun x -> (prio, apply x) in
            (prio + 1, pop :: prefixs, infixs)
        | Infix {assoc; apply} ->
            let pop = op.pop >>| fun x -> (assoc, prio, apply x) in
            (prio + 1, prefixs, pop :: infixs) )
  in

  (* Pratt parser
     https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html *)
  let rec helper min_prio =
    let pprefix =
      let* prio, apply = ws *> choice prefixs in
      let* rhs = helper prio in
      return (apply rhs)
    in
    let* lhs = pprefix <|> poprnd in

    let pinfix =
      let* assoc, prio, apply = ws *> choice infixs in
      let* () = if prio < min_prio then fail "" else unit in

      (* if left assoc then break if next operator has the same priority *)
      let prio = match assoc with `Left -> prio + 1 | `Right -> prio in
      let* rhs = helper prio in

      return (apply, rhs)
    in
    (* XXX: recursive parser with acc would be better here
       than many + fold *)
    many pinfix
    >>| List.fold ~init:lhs ~f:(fun acc (apply, oprnd) -> apply acc oprnd)
  in

  helper 0
