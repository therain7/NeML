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
  run {| let rec fact n = if n <= 1 then 1 else n * fact (n - 1) |} ;
  [%expect
    {|
    [(StrLet (Recursive,
        ({ pat = (PatVar (Id "fact"));
           expr =
           (ExpFun (((PatVar (Id "n")), []),
              (ExpIf (
                 (ExpApply (
                    (ExpApply ((ExpIdent (Id "<=")), (ExpIdent (Id "n")))),
                    (ExpConst (ConstInt 1)))),
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
           },
         [])
        ))
      ]
    |}]

(* ======= Patterns ======= *)

let%expect_test _ =
  run {| let Cons (hd, tl) = () |} ;
  [%expect
    {|
    [(StrLet (Nonrecursive,
        ({ pat =
           (PatConstruct ((Id "Cons"),
              (Some (PatTuple ((PatVar (Id "hd")), (PatVar (Id "tl")), [])))));
           expr = (ExpConstruct ((Id "()"), None)) },
         [])
        ))
      ]
    |}]

let%expect_test _ =
  run {| let C _ | a, b = () |} ;
  [%expect
    {|
    [(StrLet (Nonrecursive,
        ({ pat =
           (PatOr ((PatConstruct ((Id "C"), (Some PatAny))),
              (PatTuple ((PatVar (Id "a")), (PatVar (Id "b")), []))));
           expr = (ExpConstruct ((Id "()"), None)) },
         [])
        ))
      ]
    |}]

let%expect_test _ =
  run {| let a | (b | c) | d = () |} ;
  [%expect
    {|
    [(StrLet (Nonrecursive,
        ({ pat =
           (PatOr (
              (PatOr ((PatVar (Id "a")),
                 (PatOr ((PatVar (Id "b")), (PatVar (Id "c")))))),
              (PatVar (Id "d"))));
           expr = (ExpConstruct ((Id "()"), None)) },
         [])
        ))
      ]
    |}]

let%expect_test _ =
  run {| let a, (b, c), d = () |} ;
  [%expect
    {|
    [(StrLet (Nonrecursive,
        ({ pat =
           (PatTuple
              ((PatVar (Id "a")),
               (PatTuple ((PatVar (Id "b")), (PatVar (Id "c")), [])),
               [(PatVar (Id "d"))]));
           expr = (ExpConstruct ((Id "()"), None)) },
         [])
        ))
      ]
    |}]

let%expect_test _ =
  run {| let a, b | c, d = () |} ;
  [%expect
    {|
    [(StrLet (Nonrecursive,
        ({ pat =
           (PatOr ((PatTuple ((PatVar (Id "a")), (PatVar (Id "b")), [])),
              (PatTuple ((PatVar (Id "c")), (PatVar (Id "d")), []))));
           expr = (ExpConstruct ((Id "()"), None)) },
         [])
        ))
      ]
    |}]

let%expect_test _ =
  run {| let a::(b::c)::d = () |} ;
  [%expect
    {|
    [(StrLet (Nonrecursive,
        ({ pat =
           (PatConstruct ((Id "::"),
              (Some (PatTuple
                       ((PatVar (Id "a")),
                        (PatConstruct ((Id "::"),
                           (Some (PatTuple
                                    ((PatConstruct ((Id "::"),
                                        (Some (PatTuple
                                                 ((PatVar (Id "b")),
                                                  (PatVar (Id "c")), [])))
                                        )),
                                     (PatVar (Id "d")), [])))
                           )),
                        [])))
              ));
           expr = (ExpConstruct ((Id "()"), None)) },
         [])
        ))
      ]
    |}]

let%expect_test _ =
  run {| let a::b::c,d|e = () |} ;
  [%expect
    {|
    [(StrLet (Nonrecursive,
        ({ pat =
           (PatOr (
              (PatTuple
                 ((PatConstruct ((Id "::"),
                     (Some (PatTuple
                              ((PatVar (Id "a")),
                               (PatConstruct ((Id "::"),
                                  (Some (PatTuple
                                           ((PatVar (Id "b")), (PatVar (Id "c")),
                                            [])))
                                  )),
                               [])))
                     )),
                  (PatVar (Id "d")), [])),
              (PatVar (Id "e"))));
           expr = (ExpConstruct ((Id "()"), None)) },
         [])
        ))
      ]
    |}]

let%expect_test _ =
  run {| let [a;b;c] = () |} ;
  [%expect
    {|
    [(StrLet (Nonrecursive,
        ({ pat =
           (PatConstruct ((Id "::"),
              (Some (PatTuple
                       ((PatVar (Id "a")),
                        (PatConstruct ((Id "::"),
                           (Some (PatTuple
                                    ((PatVar (Id "b")),
                                     (PatConstruct ((Id "::"),
                                        (Some (PatTuple
                                                 ((PatVar (Id "c")),
                                                  (PatConstruct ((Id "[]"), None
                                                     )),
                                                  [])))
                                        )),
                                     [])))
                           )),
                        [])))
              ));
           expr = (ExpConstruct ((Id "()"), None)) },
         [])
        ))
      ]
    |}]

let%expect_test _ =
  run {| let [a] = () |} ;
  [%expect
    {|
    [(StrLet (Nonrecursive,
        ({ pat =
           (PatConstruct ((Id "::"),
              (Some (PatTuple
                       ((PatVar (Id "a")), (PatConstruct ((Id "[]"), None)), [])))
              ));
           expr = (ExpConstruct ((Id "()"), None)) },
         [])
        ))
      ]
    |}]

let%expect_test _ =
  run {| let [] = () |} ;
  [%expect
    {|
    [(StrLet (Nonrecursive,
        ({ pat = (PatConstruct ((Id "[]"), None));
           expr = (ExpConstruct ((Id "()"), None)) },
         [])
        ))
      ]
    |}]

let%expect_test _ =
  run {| let hd1::hd2::tl = () |} ;
  [%expect
    {|
    [(StrLet (Nonrecursive,
        ({ pat =
           (PatConstruct ((Id "::"),
              (Some (PatTuple
                       ((PatVar (Id "hd1")),
                        (PatConstruct ((Id "::"),
                           (Some (PatTuple
                                    ((PatVar (Id "hd2")), (PatVar (Id "tl")), [])))
                           )),
                        [])))
              ));
           expr = (ExpConstruct ((Id "()"), None)) },
         [])
        ))
      ]
    |}]

let%expect_test _ =
  run {| let ( x : int ) = 1 |} ;
  [%expect
    {|
    [(StrLet (Nonrecursive,
        ({ pat = (PatConstraint ((PatVar (Id "x")), (TyCon ((Id "int"), []))));
           expr = (ExpConst (ConstInt 1)) },
         [])
        ))
      ]
    |}]

let%expect_test _ =
  run {| let Some Some (x : int) = Some (Some 1) |} ;
  [%expect
    {|
    [(StrLet (Nonrecursive,
        ({ pat =
           (PatConstruct ((Id "Some"),
              (Some (PatConstruct ((Id "Some"),
                       (Some (PatConstraint ((PatVar (Id "x")),
                                (TyCon ((Id "int"), [])))))
                       )))
              ));
           expr =
           (ExpConstruct ((Id "Some"),
              (Some (ExpConstruct ((Id "Some"), (Some (ExpConst (ConstInt 1))))))
              ))
           },
         [])
        ))
      ]
    |}]

let%expect_test _ =
  run {| let Some Some x : int option option = Some (Some 1) |} ;
  [%expect
    {|
    [(StrLet (Nonrecursive,
        ({ pat =
           (PatConstraint (
              (PatConstruct ((Id "Some"),
                 (Some (PatConstruct ((Id "Some"), (Some (PatVar (Id "x")))))))),
              (TyCon ((Id "option"),
                 [(TyCon ((Id "option"), [(TyCon ((Id "int"), []))]))]))
              ));
           expr =
           (ExpConstruct ((Id "Some"),
              (Some (ExpConstruct ((Id "Some"), (Some (ExpConst (ConstInt 1))))))
              ))
           },
         [])
        ))
      ]
    |}]

(* ======= Expressions ======= *)

let%expect_test _ =
  run {| function | a -> true | b -> false |} ;
  [%expect
    {|
    [(StrEval
        (ExpFunction
           ({ left = (PatVar (Id "a"));
              right = (ExpConstruct ((Id "true"), None)) },
            [{ left = (PatVar (Id "b"));
               right = (ExpConstruct ((Id "false"), None)) }
              ])))
      ]
    |}]

let%expect_test _ =
  run {| fun x y -> x + y |} ;
  [%expect
    {|
    [(StrEval
        (ExpFun (((PatVar (Id "x")), [(PatVar (Id "y"))]),
           (ExpApply ((ExpApply ((ExpIdent (Id "+")), (ExpIdent (Id "x")))),
              (ExpIdent (Id "y"))))
           )))
      ]
    |}]

let%expect_test _ =
  run {| a0b'c_d |} ; [%expect {| [(StrEval (ExpIdent (Id "a0b'c_d")))] |}]

let%expect_test _ =
  run "a >>= b ++ c ** d !+ e" ;
  [%expect
    {|
    [(StrEval
        (ExpApply ((ExpApply ((ExpIdent (Id ">>=")), (ExpIdent (Id "a")))),
           (ExpApply ((ExpApply ((ExpIdent (Id "++")), (ExpIdent (Id "b")))),
              (ExpApply ((ExpApply ((ExpIdent (Id "**")), (ExpIdent (Id "c")))),
                 (ExpApply ((ExpIdent (Id "d")),
                    (ExpApply ((ExpIdent (Id "!+")), (ExpIdent (Id "e"))))))
                 ))
              ))
           )))
      ]
    |}]

let%expect_test _ =
  run {| let rec a = 1 and b = 2 in let e = 3 in a |} ;
  [%expect
    {|
    [(StrEval
        (ExpLet (Recursive,
           ({ pat = (PatVar (Id "a")); expr = (ExpConst (ConstInt 1)) },
            [{ pat = (PatVar (Id "b")); expr = (ExpConst (ConstInt 2)) }]),
           (ExpLet (Nonrecursive,
              ({ pat = (PatVar (Id "e")); expr = (ExpConst (ConstInt 3)) }, []),
              (ExpIdent (Id "a"))))
           )))
      ]
    |}]

let%expect_test _ =
  run {| if a then (if b then c) else d |} ;
  [%expect
    {|
    [(StrEval
        (ExpIf ((ExpIdent (Id "a")),
           (ExpIf ((ExpIdent (Id "b")), (ExpIdent (Id "c")), None)),
           (Some (ExpIdent (Id "d"))))))
      ]
    |}]

let%expect_test _ =
  run {| if a; b then c; d |} ;
  [%expect
    {|
    [(StrEval
        (ExpIf ((ExpSeq ((ExpIdent (Id "a")), (ExpIdent (Id "b")), [])),
           (ExpSeq ((ExpIdent (Id "c")), (ExpIdent (Id "d")), [])), None)))
      ]
    |}]

let%expect_test _ =
  run {| if a; b then (c; d) |} ;
  [%expect
    {|
    [(StrEval
        (ExpIf ((ExpSeq ((ExpIdent (Id "a")), (ExpIdent (Id "b")), [])),
           (ExpSeq ((ExpIdent (Id "c")), (ExpIdent (Id "d")), [])), None)))
      ]
    |}]

let%expect_test _ =
  run {| match a with b -> c | d -> e |} ;
  [%expect
    {|
    [(StrEval
        (ExpMatch ((ExpIdent (Id "a")),
           ({ left = (PatVar (Id "b")); right = (ExpIdent (Id "c")) },
            [{ left = (PatVar (Id "d")); right = (ExpIdent (Id "e")) }])
           )))
      ]
    |}]

let%expect_test _ =
  run {| match a with | b | c | d -> e | f -> g |} ;
  [%expect
    {|
    [(StrEval
        (ExpMatch ((ExpIdent (Id "a")),
           ({ left =
              (PatOr ((PatOr ((PatVar (Id "b")), (PatVar (Id "c")))),
                 (PatVar (Id "d"))));
              right = (ExpIdent (Id "e")) },
            [{ left = (PatVar (Id "f")); right = (ExpIdent (Id "g")) }])
           )))
      ]
    |}]

let%expect_test _ =
  run {| Nil |} ; [%expect {| [(StrEval (ExpConstruct ((Id "Nil"), None)))] |}]

let%expect_test _ =
  run {| Some x |} ;
  [%expect
    {| [(StrEval (ExpConstruct ((Id "Some"), (Some (ExpIdent (Id "x"))))))] |}]

let%expect_test _ =
  run {| Cons (1, Nil) |} ;
  [%expect
    {|
    [(StrEval
        (ExpConstruct ((Id "Cons"),
           (Some (ExpTuple
                    ((ExpConst (ConstInt 1)), (ExpConstruct ((Id "Nil"), None)),
                     [])))
           )))
      ]
    |}]

let%expect_test _ =
  run {| [a;b;c] |} ;
  [%expect
    {|
    [(StrEval
        (ExpConstruct ((Id "::"),
           (Some (ExpTuple
                    ((ExpIdent (Id "a")),
                     (ExpConstruct ((Id "::"),
                        (Some (ExpTuple
                                 ((ExpIdent (Id "b")),
                                  (ExpConstruct ((Id "::"),
                                     (Some (ExpTuple
                                              ((ExpIdent (Id "c")),
                                               (ExpConstruct ((Id "[]"), None)),
                                               [])))
                                     )),
                                  [])))
                        )),
                     [])))
           )))
      ]
    |}]

let%expect_test _ =
  run {| [a;(b;c)] |} ;
  [%expect
    {|
    [(StrEval
        (ExpConstruct ((Id "::"),
           (Some (ExpTuple
                    ((ExpIdent (Id "a")),
                     (ExpConstruct ((Id "::"),
                        (Some (ExpTuple
                                 ((ExpSeq
                                     ((ExpIdent (Id "b")), (ExpIdent (Id "c")),
                                      [])),
                                  (ExpConstruct ((Id "[]"), None)), [])))
                        )),
                     [])))
           )))
      ]
    |}]

let%expect_test _ =
  run {| [a] |} ;
  [%expect
    {|
    [(StrEval
        (ExpConstruct ((Id "::"),
           (Some (ExpTuple
                    ((ExpIdent (Id "a")), (ExpConstruct ((Id "[]"), None)), [])))
           )))
      ]
    |}]

let%expect_test _ =
  run {| [] |} ; [%expect {| [(StrEval (ExpConstruct ((Id "[]"), None)))] |}]

let%expect_test _ =
  run {| (a :: b) :: c :: d :: [] |} ;
  [%expect
    {|
    [(StrEval
        (ExpConstruct ((Id "::"),
           (Some (ExpTuple
                    ((ExpConstruct ((Id "::"),
                        (Some (ExpTuple
                                 ((ExpIdent (Id "a")), (ExpIdent (Id "b")), [])))
                        )),
                     (ExpConstruct ((Id "::"),
                        (Some (ExpTuple
                                 ((ExpIdent (Id "c")),
                                  (ExpConstruct ((Id "::"),
                                     (Some (ExpTuple
                                              ((ExpIdent (Id "d")),
                                               (ExpConstruct ((Id "[]"), None)),
                                               [])))
                                     )),
                                  [])))
                        )),
                     [])))
           )))
      ]
    |}]

let%expect_test _ =
  run {| (a ; b) ; c ; d ; e |} ;
  [%expect
    {|
    [(StrEval
        (ExpSeq
           ((ExpSeq ((ExpIdent (Id "a")), (ExpIdent (Id "b")), [])),
            (ExpIdent (Id "c")), [(ExpIdent (Id "d")); (ExpIdent (Id "e"))])))
      ]
    |}]

let%expect_test _ =
  run {| a, (b, c), d, e |} ;
  [%expect
    {|
    [(StrEval
        (ExpTuple
           ((ExpIdent (Id "a")),
            (ExpTuple ((ExpIdent (Id "b")), (ExpIdent (Id "c")), [])),
            [(ExpIdent (Id "d")); (ExpIdent (Id "e"))])))
      ]
    |}]

let%expect_test _ =
  run {| a, (b, c) |} ;
  [%expect
    {|
    [(StrEval
        (ExpTuple
           ((ExpIdent (Id "a")),
            (ExpTuple ((ExpIdent (Id "b")), (ExpIdent (Id "c")), [])), [])))
      ]
    |}]

let%expect_test _ =
  run {| (a, b), c |} ;
  [%expect
    {|
    [(StrEval
        (ExpTuple
           ((ExpTuple ((ExpIdent (Id "a")), (ExpIdent (Id "b")), [])),
            (ExpIdent (Id "c")), [])))
      ]
    |}]

let%expect_test _ =
  run {| 1 + - + + 3 |} ;
  [%expect
    {|
    [(StrEval
        (ExpApply ((ExpApply ((ExpIdent (Id "+")), (ExpConst (ConstInt 1)))),
           (ExpApply ((ExpIdent (Id "~-")),
              (ExpApply ((ExpIdent (Id "~+")),
                 (ExpApply ((ExpIdent (Id "~+")), (ExpConst (ConstInt 3))))))
              ))
           )))
      ]
    |}]

let%expect_test _ =
  run {| !%< 123; !0 |} ;
  [%expect
    {|
    [(StrEval
        (ExpSeq
           ((ExpApply ((ExpIdent (Id "!%<")), (ExpConst (ConstInt 123)))),
            (ExpApply ((ExpIdent (Id "!")), (ExpConst (ConstInt 0)))), [])))
      ]
    |}]

let%expect_test _ =
  run {| --+1 |} ;
  [%expect
    {|
    [(StrEval
        (ExpApply ((ExpIdent (Id "~-")),
           (ExpApply ((ExpIdent (Id "~-")),
              (ExpApply ((ExpIdent (Id "~+")), (ExpConst (ConstInt 1))))))
           )))
      ]
    |}]

let%expect_test _ =
  run {| f(1+2+3) |} ;
  [%expect
    {|
    [(StrEval
        (ExpApply ((ExpIdent (Id "f")),
           (ExpApply (
              (ExpApply ((ExpIdent (Id "+")),
                 (ExpApply (
                    (ExpApply ((ExpIdent (Id "+")), (ExpConst (ConstInt 1)))),
                    (ExpConst (ConstInt 2))))
                 )),
              (ExpConst (ConstInt 3))))
           )))
      ]
    |}]

let%expect_test _ =
  run {| if(a && b) then(1+2) else(3) |} ;
  [%expect
    {|
    [(StrEval
        (ExpIf (
           (ExpApply ((ExpApply ((ExpIdent (Id "&&")), (ExpIdent (Id "a")))),
              (ExpIdent (Id "b")))),
           (ExpApply ((ExpApply ((ExpIdent (Id "+")), (ExpConst (ConstInt 1)))),
              (ExpConst (ConstInt 2)))),
           (Some (ExpConst (ConstInt 3))))))
      ]
    |}]

let%expect_test _ =
  run {| id let a = 1 in a |} ;
  [%expect
    {|
    [(StrEval
        (ExpApply ((ExpIdent (Id "id")),
           (ExpLet (Nonrecursive,
              ({ pat = (PatVar (Id "a")); expr = (ExpConst (ConstInt 1)) }, []),
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
              ({ pat = (PatVar (Id "a")); expr = (ExpConst (ConstInt 1)) }, []),
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
              ({ pat = (PatVar (Id "a")); expr = (ExpConst (ConstInt 1)) }, []),
              (ExpIdent (Id "a"))))
           )))
      ]
    |}]

let%expect_test _ =
  run {| ( a : int ) |} ;
  [%expect
    {| [(StrEval (ExpConstraint ((ExpIdent (Id "a")), (TyCon ((Id "int"), [])))))] |}]

let%expect_test _ =
  run {| (fun x -> x : int -> int) |} ;
  [%expect
    {|
    [(StrEval
        (ExpConstraint ((ExpFun (((PatVar (Id "x")), []), (ExpIdent (Id "x")))),
           (TyArr ((TyCon ((Id "int"), [])), (TyCon ((Id "int"), [])))))))
      ]
    |}]

let%expect_test _ =
  run {| let f x y : int = 1 in f |} ;
  [%expect
    {|
    [(StrEval
        (ExpLet (Nonrecursive,
           ({ pat = (PatVar (Id "f"));
              expr =
              (ExpFun (((PatVar (Id "x")), [(PatVar (Id "y"))]),
                 (ExpConstraint ((ExpConst (ConstInt 1)),
                    (TyCon ((Id "int"), []))))
                 ))
              },
            []),
           (ExpIdent (Id "f")))))
      ]
    |}]

(* ======= Types ======= *)

let%expect_test _ =
  run {| type foo = A of int |} ;
  [%expect
    {|
    [(StrType
        { id = (Id "foo"); params = [];
          variants = [{ id = (Id "A"); arg = (Some (TyCon ((Id "int"), []))) }] })
      ]
    |}]

let%expect_test _ =
  run {| type foo = A of int list |} ;
  [%expect
    {|
    [(StrType
        { id = (Id "foo"); params = [];
          variants =
          [{ id = (Id "A");
             arg = (Some (TyCon ((Id "list"), [(TyCon ((Id "int"), []))]))) }
            ]
          })
      ]
    |}]

let%expect_test _ =
  run {| type foo = A of (int, string) map |} ;
  [%expect
    {|
    [(StrType
        { id = (Id "foo"); params = [];
          variants =
          [{ id = (Id "A");
             arg =
             (Some (TyCon ((Id "map"),
                      [(TyCon ((Id "int"), [])); (TyCon ((Id "string"), []))])))
             }
            ]
          })
      ]
    |}]

let%expect_test _ =
  run {| type foo = A of 'a -> 'b -> 'c |} ;
  [%expect
    {|
    [(StrType
        { id = (Id "foo"); params = [];
          variants =
          [{ id = (Id "A");
             arg =
             (Some (TyArr ((TyVar (Id "a")),
                      (TyArr ((TyVar (Id "b")), (TyVar (Id "c")))))))
             }
            ]
          })
      ]
    |}]

let%expect_test _ =
  run {| type foo = A of 'a * 'b * 'c |} ;
  [%expect
    {|
    [(StrType
        { id = (Id "foo"); params = [];
          variants =
          [{ id = (Id "A");
             arg =
             (Some (TyTuple
                      ((TyVar (Id "a")), (TyVar (Id "b")), [(TyVar (Id "c"))])))
             }
            ]
          })
      ]
    |}]

let%expect_test _ =
  run {| type foo = A of 'some_type_var |} ;
  [%expect
    {|
    [(StrType
        { id = (Id "foo"); params = [];
          variants =
          [{ id = (Id "A"); arg = (Some (TyVar (Id "some_type_var"))) }] })
      ]
    |}]

let%expect_test _ =
  run
    {| type foo = A of 
         ('a -> int * (string, unit, 'b -> 'c) foo bar option) -> e |} ;
  [%expect
    {|
    [(StrType
        { id = (Id "foo"); params = [];
          variants =
          [{ id = (Id "A");
             arg =
             (Some (TyArr (
                      (TyArr ((TyVar (Id "a")),
                         (TyTuple
                            ((TyCon ((Id "int"), [])),
                             (TyCon ((Id "option"),
                                [(TyCon ((Id "bar"),
                                    [(TyCon ((Id "foo"),
                                        [(TyCon ((Id "string"), []));
                                          (TyCon ((Id "unit"), []));
                                          (TyArr ((TyVar (Id "b")),
                                             (TyVar (Id "c"))))
                                          ]
                                        ))
                                      ]
                                    ))
                                  ]
                                )),
                             []))
                         )),
                      (TyCon ((Id "e"), [])))))
             }
            ]
          })
      ]
    |}]

(* ======= Some other stuff ======= *)

let%expect_test _ =
  run {| let (f, s) = (f + s, f - s) |} ;
  [%expect
    {|
    [(StrLet (Nonrecursive,
        ({ pat = (PatTuple ((PatVar (Id "f")), (PatVar (Id "s")), []));
           expr =
           (ExpTuple
              ((ExpApply ((ExpApply ((ExpIdent (Id "+")), (ExpIdent (Id "f")))),
                  (ExpIdent (Id "s")))),
               (ExpApply ((ExpApply ((ExpIdent (Id "-")), (ExpIdent (Id "f")))),
                  (ExpIdent (Id "s")))),
               []))
           },
         [])
        ))
      ]
    |}]

let%expect_test _ =
  run {| let (>>=) a b = a ** b |} ;
  [%expect
    {|
    [(StrLet (Nonrecursive,
        ({ pat = (PatVar (Id ">>="));
           expr =
           (ExpFun (((PatVar (Id "a")), [(PatVar (Id "b"))]),
              (ExpApply ((ExpApply ((ExpIdent (Id "**")), (ExpIdent (Id "a")))),
                 (ExpIdent (Id "b"))))
              ))
           },
         [])
        ))
      ]
    |}]

let%expect_test _ =
  run {| let (++) a b = a + b |} ;
  [%expect
    {|
    [(StrLet (Nonrecursive,
        ({ pat = (PatVar (Id "++"));
           expr =
           (ExpFun (((PatVar (Id "a")), [(PatVar (Id "b"))]),
              (ExpApply ((ExpApply ((ExpIdent (Id "+")), (ExpIdent (Id "a")))),
                 (ExpIdent (Id "b"))))
              ))
           },
         [])
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
        ({ pat = (PatVar (Id "f"));
           expr =
           (ExpFun (((PatVar (Id "n")), []),
              (ExpApply ((ExpApply ((ExpIdent (Id "+")), (ExpIdent (Id "n")))),
                 (ExpConst (ConstInt 1))))
              ))
           },
         [])
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
        ({ pat = (PatVar (Id "reca")); expr = (ExpConst (ConstInt 1)) }, [])))
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
                       ((TyVar (Id "a")),
                        (TyCon ((Id "list"), [(TyVar (Id "a"))])), [])))
              }
            ]
          })
      ]
    |}]

let%expect_test _ = run {| 1a |} ; [%expect {| syntax error |}]

let%expect_test _ =
  run {| 1 ;; a |} ;
  [%expect
    {| [(StrEval (ExpConst (ConstInt 1))); (StrEval (ExpIdent (Id "a")))] |}]
