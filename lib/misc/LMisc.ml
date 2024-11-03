[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base

(** Identifiers *)
module Id = struct
  type t = I of string [@@deriving show {with_path= false}]
end

(** List containing at least 1 element *)
module List1 = struct
  type 'a t = 'a * 'a list [@@deriving show {with_path= false}]

  let of_list_exn : 'a list -> 'a t = function
    | hd :: tl ->
        (hd, tl)
    | [] ->
        raise (Invalid_argument "empty list")
end

(** List containing at least 2 elements *)
module List2 = struct
  type 'a t = 'a * 'a * 'a list [@@deriving show {with_path= false}]

  let of_list_exn : 'a list -> 'a t = function
    | fst :: snd :: tl ->
        (fst, snd, tl)
    | _ :: [] | [] ->
        raise (Invalid_argument "not enough elements")
end
