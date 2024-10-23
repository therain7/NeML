[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base
open Stdio
open LAst

let run s =
  match LParse.parse s with
  | None ->
      print_endline "syntax error"
  | Some str ->
      print_endline (show_structure str)

let%expect_test _ =
  run {| let rec fact n = if n < 2 then 1 else n * fact (n - 1) |} ;
  [%expect
    {|
    [(StrLet (Recursive,
        [{ pat = (PatVar (Id "fact"));
           expr =
           (ExpFun ([(PatVar (Id "n"))],
              (ExpIfThenElse (
                 (ExpApply (
                    (ExpApply ((ExpIdent (Id "<")), (ExpIdent (Id "n")))),
                    (ExpConst (ConstInt 2)))),
                 (ExpConst (ConstInt 1)),
                 (Some (ExpApply (
                          (ExpApply ((ExpIdent (Id "*")), (ExpIdent (Id "n")))),
                          (ExpApply ((ExpIdent (Id "fact")),
                             (ExpApply (
                                (ExpApply ((ExpIdent (Id "-")),
                                   (ExpIdent (Id "n")))),
                                (ExpConst (ConstInt 1))))
                             ))
                          )))
                 ))
              ))
           }
          ]
        ))
      ]
    |}]

let%expect_test _ =
  run {| let hd1::hd2::tl = list |} ;
  [%expect
    {|
    [(StrLet (Nonrecursive,
        [{ pat =
           (PatConstruct ((Id "::"),
              (Some (PatTuple
                       [(PatVar (Id "hd1"));
                         (PatConstruct ((Id "::"),
                            (Some (PatTuple
                                     [(PatVar (Id "hd2")); (PatVar (Id "tl"))]))
                            ))
                         ]))
              ));
           expr = (ExpIdent (Id "list")) }
          ]
        ))
      ]
    |}]

let%expect_test _ =
  run {| let (f, s) = (f + s, f - s) |} ;
  [%expect
    {|
    [(StrLet (Nonrecursive,
        [{ pat = (PatTuple [(PatVar (Id "f")); (PatVar (Id "s"))]);
           expr =
           (ExpTuple
              [(ExpApply ((ExpApply ((ExpIdent (Id "+")), (ExpIdent (Id "f")))),
                  (ExpIdent (Id "s"))));
                (ExpApply ((ExpApply ((ExpIdent (Id "-")), (ExpIdent (Id "f")))),
                   (ExpIdent (Id "s"))))
                ])
           }
          ]
        ))
      ]
    |}]

let%expect_test _ =
  run {| let (>>=) a b = a ** b |} ;
  [%expect
    {|
    [(StrLet (Nonrecursive,
        [{ pat = (PatVar (Id ">>="));
           expr =
           (ExpFun ([(PatVar (Id "a")); (PatVar (Id "b"))],
              (ExpApply ((ExpApply ((ExpIdent (Id "**")), (ExpIdent (Id "a")))),
                 (ExpIdent (Id "b"))))
              ))
           }
          ]
        ))
      ]
    |}]

let%expect_test _ =
  run {| let (++) a b = a + b |} ;
  [%expect
    {|
    [(StrLet (Nonrecursive,
        [{ pat = (PatVar (Id "++"));
           expr =
           (ExpFun ([(PatVar (Id "a")); (PatVar (Id "b"))],
              (ExpApply ((ExpApply ((ExpIdent (Id "+")), (ExpIdent (Id "a")))),
                 (ExpIdent (Id "b"))))
              ))
           }
          ]
        ))
      ]
    |}]

let%expect_test _ =
  run
    {| let(*sus*)rec(*firstcomment*)f n = (* second comment *) (* third
         comment*) n + 1 |} ;
  [%expect
    {|
    [(StrLet (Recursive,
        [{ pat = (PatVar (Id "f"));
           expr =
           (ExpFun ([(PatVar (Id "n"))],
              (ExpApply ((ExpApply ((ExpIdent (Id "+")), (ExpIdent (Id "n")))),
                 (ExpConst (ConstInt 1))))
              ))
           }
          ]
        ))
      ]
    |}]

let%expect_test _ =
  run {| letrec f n = n + 1 |} ;
  [%expect
    {|
    [(StrEval
        (ExpApply (
           (ExpApply ((ExpIdent (Id "=")),
              (ExpApply (
                 (ExpApply ((ExpIdent (Id "letrec")), (ExpIdent (Id "f")))),
                 (ExpIdent (Id "n"))))
              )),
           (ExpApply ((ExpApply ((ExpIdent (Id "+")), (ExpIdent (Id "n")))),
              (ExpConst (ConstInt 1))))
           )))
      ]
    |}]

let%expect_test _ =
  run {| let reca = 1 |} ;
  [%expect
    {|
    [(StrLet (Nonrecursive,
        [{ pat = (PatVar (Id "reca")); expr = (ExpConst (ConstInt 1)) }]))
      ]
    |}]

let%expect_test _ =
  run {| type 'a list = Nil | Cons of 'a * 'a list |} ;
  [%expect
    {|
    [(StrType
        { id = (Id "list"); params = [(Id "a")];
          variants =
          [{ id = (Id "Nil"); arg = None };
            { id = (Id "Cons");
              arg =
              (Some (TyTuple
                       [(TyVar (Id "a"));
                         (TyCon ((Id "list"), [(TyVar (Id "a"))]))]))
              }
            ]
          })
      ]
    |}]

let%expect_test _ =
  run {| id let a = 1 in a |} ;
  [%expect
    {|
    [(StrEval
        (ExpApply ((ExpIdent (Id "id")),
           (ExpLet (Nonrecursive,
              [{ pat = (PatVar (Id "a")); expr = (ExpConst (ConstInt 1)) }],
              (ExpIdent (Id "a"))))
           )))
      ]
    |}]

let%expect_test _ =
  run {| ! let a = 1 in a |} ;
  [%expect
    {|
    [(StrEval
        (ExpApply ((ExpIdent (Id "!")),
           (ExpLet (Nonrecursive,
              [{ pat = (PatVar (Id "a")); expr = (ExpConst (ConstInt 1)) }],
              (ExpIdent (Id "a"))))
           )))
      ]
    |}]

let%expect_test _ =
  run {| 1 + let a = 1 in a |} ;
  [%expect
    {|
    [(StrEval
        (ExpApply ((ExpApply ((ExpIdent (Id "+")), (ExpConst (ConstInt 1)))),
           (ExpLet (Nonrecursive,
              [{ pat = (PatVar (Id "a")); expr = (ExpConst (ConstInt 1)) }],
              (ExpIdent (Id "a"))))
           )))
      ]
    |}]
