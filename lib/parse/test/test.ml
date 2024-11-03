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
    [(Let (Rec,
        ({ pat = (Var (I "fact"));
           expr =
           (Fun (((Var (I "n")), []),
              (If (
                 (Apply ((Apply ((Id (I "<=")), (Id (I "n")))), (Const (Int 1)))),
                 (Const (Int 1)),
                 (Some (Apply ((Apply ((Id (I "*")), (Id (I "n")))),
                          (Apply ((Id (I "fact")),
                             (Apply ((Apply ((Id (I "-")), (Id (I "n")))),
                                (Const (Int 1))))
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
    [(Let (Nonrec,
        ({ pat =
           (Construct ((I "Cons"),
              (Some (Tuple ((Var (I "hd")), (Var (I "tl")), [])))));
           expr = (Construct ((I "()"), None)) },
         [])
        ))
      ]
    |}]

let%expect_test _ =
  run {| let C _ | a, b = () |} ;
  [%expect
    {|
    [(Let (Nonrec,
        ({ pat =
           (Or ((Construct ((I "C"), (Some Any))),
              (Tuple ((Var (I "a")), (Var (I "b")), []))));
           expr = (Construct ((I "()"), None)) },
         [])
        ))
      ]
    |}]

let%expect_test _ =
  run {| let a | (b | c) | d = () |} ;
  [%expect
    {|
    [(Let (Nonrec,
        ({ pat =
           (Or ((Or ((Var (I "a")), (Or ((Var (I "b")), (Var (I "c")))))),
              (Var (I "d"))));
           expr = (Construct ((I "()"), None)) },
         [])
        ))
      ]
    |}]

let%expect_test _ =
  run {| let a, (b, c), d = () |} ;
  [%expect
    {|
    [(Let (Nonrec,
        ({ pat =
           (Tuple
              ((Var (I "a")), (Tuple ((Var (I "b")), (Var (I "c")), [])),
               [(Var (I "d"))]));
           expr = (Construct ((I "()"), None)) },
         [])
        ))
      ]
    |}]

let%expect_test _ =
  run {| let a, b | c, d = () |} ;
  [%expect
    {|
    [(Let (Nonrec,
        ({ pat =
           (Or ((Tuple ((Var (I "a")), (Var (I "b")), [])),
              (Tuple ((Var (I "c")), (Var (I "d")), []))));
           expr = (Construct ((I "()"), None)) },
         [])
        ))
      ]
    |}]

let%expect_test _ =
  run {| let a::(b::c)::d = () |} ;
  [%expect
    {|
    [(Let (Nonrec,
        ({ pat =
           (Construct ((I "::"),
              (Some (Tuple
                       ((Var (I "a")),
                        (Construct ((I "::"),
                           (Some (Tuple
                                    ((Construct ((I "::"),
                                        (Some (Tuple
                                                 ((Var (I "b")), (Var (I "c")),
                                                  [])))
                                        )),
                                     (Var (I "d")), [])))
                           )),
                        [])))
              ));
           expr = (Construct ((I "()"), None)) },
         [])
        ))
      ]
    |}]

let%expect_test _ =
  run {| let a::b::c,d|e = () |} ;
  [%expect
    {|
    [(Let (Nonrec,
        ({ pat =
           (Or (
              (Tuple
                 ((Construct ((I "::"),
                     (Some (Tuple
                              ((Var (I "a")),
                               (Construct ((I "::"),
                                  (Some (Tuple ((Var (I "b")), (Var (I "c")), [])))
                                  )),
                               [])))
                     )),
                  (Var (I "d")), [])),
              (Var (I "e"))));
           expr = (Construct ((I "()"), None)) },
         [])
        ))
      ]
    |}]

let%expect_test _ =
  run {| let [a;b;c] = () |} ;
  [%expect
    {|
    [(Let (Nonrec,
        ({ pat =
           (Construct ((I "::"),
              (Some (Tuple
                       ((Var (I "a")),
                        (Construct ((I "::"),
                           (Some (Tuple
                                    ((Var (I "b")),
                                     (Construct ((I "::"),
                                        (Some (Tuple
                                                 ((Var (I "c")),
                                                  (Construct ((I "[]"), None)),
                                                  [])))
                                        )),
                                     [])))
                           )),
                        [])))
              ));
           expr = (Construct ((I "()"), None)) },
         [])
        ))
      ]
    |}]

let%expect_test _ =
  run {| let [a] = () |} ;
  [%expect
    {|
    [(Let (Nonrec,
        ({ pat =
           (Construct ((I "::"),
              (Some (Tuple ((Var (I "a")), (Construct ((I "[]"), None)), [])))));
           expr = (Construct ((I "()"), None)) },
         [])
        ))
      ]
    |}]

let%expect_test _ =
  run {| let [] = () |} ;
  [%expect
    {|
    [(Let (Nonrec,
        ({ pat = (Construct ((I "[]"), None));
           expr = (Construct ((I "()"), None)) },
         [])
        ))
      ]
    |}]

let%expect_test _ =
  run {| let hd1::hd2::tl = () |} ;
  [%expect
    {|
    [(Let (Nonrec,
        ({ pat =
           (Construct ((I "::"),
              (Some (Tuple
                       ((Var (I "hd1")),
                        (Construct ((I "::"),
                           (Some (Tuple ((Var (I "hd2")), (Var (I "tl")), []))))),
                        [])))
              ));
           expr = (Construct ((I "()"), None)) },
         [])
        ))
      ]
    |}]

let%expect_test _ =
  run {| let ( x : int ) = 1 |} ;
  [%expect
    {|
    [(Let (Nonrec,
        ({ pat = (Constraint ((Var (I "x")), (Con ((I "int"), []))));
           expr = (Const (Int 1)) },
         [])
        ))
      ]
    |}]

let%expect_test _ =
  run {| let Some Some (x : int) = Some (Some 1) |} ;
  [%expect
    {|
    [(Let (Nonrec,
        ({ pat =
           (Construct ((I "Some"),
              (Some (Construct ((I "Some"),
                       (Some (Constraint ((Var (I "x")), (Con ((I "int"), [])))))
                       )))
              ));
           expr =
           (Construct ((I "Some"),
              (Some (Construct ((I "Some"), (Some (Const (Int 1))))))))
           },
         [])
        ))
      ]
    |}]

let%expect_test _ =
  run {| let Some Some x : int option option = Some (Some 1) |} ;
  [%expect
    {|
    [(Let (Nonrec,
        ({ pat =
           (Constraint (
              (Construct ((I "Some"),
                 (Some (Construct ((I "Some"), (Some (Var (I "x")))))))),
              (Con ((I "option"), [(Con ((I "option"), [(Con ((I "int"), []))]))]
                 ))
              ));
           expr =
           (Construct ((I "Some"),
              (Some (Construct ((I "Some"), (Some (Const (Int 1))))))))
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
    [(Eval
        (Function
           ({ left = (Var (I "a")); right = (Construct ((I "true"), None)) },
            [{ left = (Var (I "b")); right = (Construct ((I "false"), None)) }])))
      ]
    |}]

let%expect_test _ =
  run {| fun x y -> x + y |} ;
  [%expect
    {|
    [(Eval
        (Fun (((Var (I "x")), [(Var (I "y"))]),
           (Apply ((Apply ((Id (I "+")), (Id (I "x")))), (Id (I "y")))))))
      ]
    |}]

let%expect_test _ =
  run {| a0b'c_d |} ; [%expect {| [(Eval (Id (I "a0b'c_d")))] |}]

let%expect_test _ =
  run "a >>= b ++ c ** d !+ e" ;
  [%expect
    {|
    [(Eval
        (Apply ((Apply ((Id (I ">>=")), (Id (I "a")))),
           (Apply ((Apply ((Id (I "++")), (Id (I "b")))),
              (Apply ((Apply ((Id (I "**")), (Id (I "c")))),
                 (Apply ((Id (I "d")), (Apply ((Id (I "!+")), (Id (I "e"))))))))
              ))
           )))
      ]
    |}]

let%expect_test _ =
  run {| let rec a = 1 and b = 2 in let e = 3 in a |} ;
  [%expect
    {|
    [(Eval
        (Let (Rec,
           ({ pat = (Var (I "a")); expr = (Const (Int 1)) },
            [{ pat = (Var (I "b")); expr = (Const (Int 2)) }]),
           (Let (Nonrec, ({ pat = (Var (I "e")); expr = (Const (Int 3)) }, []),
              (Id (I "a"))))
           )))
      ]
    |}]

let%expect_test _ =
  run {| if a then (if b then c) else d |} ;
  [%expect
    {|
    [(Eval
        (If ((Id (I "a")), (If ((Id (I "b")), (Id (I "c")), None)),
           (Some (Id (I "d"))))))
      ]
    |}]

let%expect_test _ =
  run {| if a; b then c; d |} ;
  [%expect
    {|
    [(Eval
        (If ((Tuple ((Id (I "a")), (Id (I "b")), [])),
           (Tuple ((Id (I "c")), (Id (I "d")), [])), None)))
      ]
    |}]

let%expect_test _ =
  run {| if a; b then (c; d) |} ;
  [%expect
    {|
    [(Eval
        (If ((Tuple ((Id (I "a")), (Id (I "b")), [])),
           (Tuple ((Id (I "c")), (Id (I "d")), [])), None)))
      ]
    |}]

let%expect_test _ =
  run {| match a with b -> c | d -> e |} ;
  [%expect
    {|
    [(Eval
        (Match ((Id (I "a")),
           ({ left = (Var (I "b")); right = (Id (I "c")) },
            [{ left = (Var (I "d")); right = (Id (I "e")) }])
           )))
      ]
    |}]

let%expect_test _ =
  run {| match a with | b | c | d -> e | f -> g |} ;
  [%expect
    {|
    [(Eval
        (Match ((Id (I "a")),
           ({ left = (Or ((Or ((Var (I "b")), (Var (I "c")))), (Var (I "d"))));
              right = (Id (I "e")) },
            [{ left = (Var (I "f")); right = (Id (I "g")) }])
           )))
      ]
    |}]

let%expect_test _ =
  run {| Nil |} ; [%expect {| [(Eval (Construct ((I "Nil"), None)))] |}]

let%expect_test _ =
  run {| Some x |} ;
  [%expect {| [(Eval (Construct ((I "Some"), (Some (Id (I "x"))))))] |}]

let%expect_test _ =
  run {| Cons (1, Nil) |} ;
  [%expect
    {|
    [(Eval
        (Construct ((I "Cons"),
           (Some (Tuple ((Const (Int 1)), (Construct ((I "Nil"), None)), []))))))
      ]
    |}]

let%expect_test _ =
  run {| [a;b;c] |} ;
  [%expect
    {|
    [(Eval
        (Construct ((I "::"),
           (Some (Tuple
                    ((Tuple ((Id (I "a")), (Id (I "b")), [(Id (I "c"))])),
                     (Construct ((I "[]"), None)), [])))
           )))
      ]
    |}]

let%expect_test _ =
  run {| [a;(b;c)] |} ;
  [%expect
    {|
    [(Eval
        (Construct ((I "::"),
           (Some (Tuple
                    ((Tuple
                        ((Id (I "a")), (Tuple ((Id (I "b")), (Id (I "c")), [])),
                         [])),
                     (Construct ((I "[]"), None)), [])))
           )))
      ]
    |}]

let%expect_test _ =
  run {| [a] |} ;
  [%expect
    {|
    [(Eval
        (Construct ((I "::"),
           (Some (Tuple ((Id (I "a")), (Construct ((I "[]"), None)), []))))))
      ]
    |}]

let%expect_test _ =
  run {| [] |} ; [%expect {| [(Eval (Construct ((I "[]"), None)))] |}]

let%expect_test _ =
  run {| (a :: b) :: c :: d :: [] |} ;
  [%expect
    {|
    [(Eval
        (Construct ((I "::"),
           (Some (Tuple
                    ((Construct ((I "::"),
                        (Some (Tuple ((Id (I "a")), (Id (I "b")), []))))),
                     (Construct ((I "::"),
                        (Some (Tuple
                                 ((Id (I "c")),
                                  (Construct ((I "::"),
                                     (Some (Tuple
                                              ((Id (I "d")),
                                               (Construct ((I "[]"), None)),
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
    [(Eval
        (Tuple
           ((Tuple ((Id (I "a")), (Id (I "b")), [])), (Id (I "c")),
            [(Id (I "d")); (Id (I "e"))])))
      ]
    |}]

let%expect_test _ =
  run {| a, (b, c), d, e |} ;
  [%expect
    {|
    [(Eval
        (Tuple
           ((Id (I "a")), (Tuple ((Id (I "b")), (Id (I "c")), [])),
            [(Id (I "d")); (Id (I "e"))])))
      ]
    |}]

let%expect_test _ =
  run {| a, (b, c) |} ;
  [%expect
    {| [(Eval (Tuple ((Id (I "a")), (Tuple ((Id (I "b")), (Id (I "c")), [])), [])))] |}]

let%expect_test _ =
  run {| (a, b), c |} ;
  [%expect
    {| [(Eval (Tuple ((Tuple ((Id (I "a")), (Id (I "b")), [])), (Id (I "c")), [])))] |}]

let%expect_test _ =
  run {| 1 + - + + 3 |} ;
  [%expect
    {|
    [(Eval
        (Apply ((Apply ((Id (I "+")), (Const (Int 1)))),
           (Apply ((Id (I "~-")),
              (Apply ((Id (I "~+")), (Apply ((Id (I "~+")), (Const (Int 3))))))))
           )))
      ]
    |}]

let%expect_test _ =
  run {| !%< 123; !0 |} ;
  [%expect
    {|
    [(Eval
        (Tuple
           ((Apply ((Id (I "!%<")), (Const (Int 123)))),
            (Apply ((Id (I "!")), (Const (Int 0)))), [])))
      ]
    |}]

let%expect_test _ =
  run {| --+1 |} ;
  [%expect
    {|
    [(Eval
        (Apply ((Id (I "~-")),
           (Apply ((Id (I "~-")), (Apply ((Id (I "~+")), (Const (Int 1)))))))))
      ]
    |}]

let%expect_test _ =
  run {| f(1+2+3) |} ;
  [%expect
    {|
    [(Eval
        (Apply ((Id (I "f")),
           (Apply (
              (Apply ((Id (I "+")),
                 (Apply ((Apply ((Id (I "+")), (Const (Int 1)))), (Const (Int 2))
                    ))
                 )),
              (Const (Int 3))))
           )))
      ]
    |}]

let%expect_test _ =
  run {| if(a && b) then(1+2) else(3) |} ;
  [%expect
    {|
    [(Eval
        (If ((Apply ((Apply ((Id (I "&&")), (Id (I "a")))), (Id (I "b")))),
           (Apply ((Apply ((Id (I "+")), (Const (Int 1)))), (Const (Int 2)))),
           (Some (Const (Int 3))))))
      ]
    |}]

let%expect_test _ =
  run {| id let a = 1 in a |} ;
  [%expect
    {|
    [(Eval
        (Apply ((Id (I "id")),
           (Let (Nonrec, ({ pat = (Var (I "a")); expr = (Const (Int 1)) }, []),
              (Id (I "a"))))
           )))
      ]
    |}]

let%expect_test _ =
  run {| ! let a = 1 in a |} ;
  [%expect
    {|
    [(Eval
        (Apply ((Id (I "!")),
           (Let (Nonrec, ({ pat = (Var (I "a")); expr = (Const (Int 1)) }, []),
              (Id (I "a"))))
           )))
      ]
    |}]

let%expect_test _ =
  run {| 1 + let a = 1 in a |} ;
  [%expect
    {|
    [(Eval
        (Apply ((Apply ((Id (I "+")), (Const (Int 1)))),
           (Let (Nonrec, ({ pat = (Var (I "a")); expr = (Const (Int 1)) }, []),
              (Id (I "a"))))
           )))
      ]
    |}]

let%expect_test _ =
  run {| ( a : int ) |} ;
  [%expect {| [(Eval (Constraint ((Id (I "a")), (Con ((I "int"), [])))))] |}]

let%expect_test _ =
  run {| (fun x -> x : int -> int) |} ;
  [%expect
    {|
    [(Eval
        (Constraint ((Fun (((Var (I "x")), []), (Id (I "x")))),
           (Arr ((Con ((I "int"), [])), (Con ((I "int"), [])))))))
      ]
    |}]

let%expect_test _ =
  run {| let f x y : int = 1 in f |} ;
  [%expect
    {|
    [(Eval
        (Let (Nonrec,
           ({ pat = (Var (I "f"));
              expr =
              (Fun (((Var (I "x")), [(Var (I "y"))]),
                 (Constraint ((Const (Int 1)), (Con ((I "int"), []))))))
              },
            []),
           (Id (I "f")))))
      ]
    |}]

(* ======= Types ======= *)

let%expect_test _ =
  run {| type foo = A of int |} ;
  [%expect
    {|
    [(Type
        { id = (I "foo"); params = [];
          variants = [{ id = (I "A"); arg = (Some (Con ((I "int"), []))) }] })
      ]
    |}]

let%expect_test _ =
  run {| type foo = A of int list |} ;
  [%expect
    {|
    [(Type
        { id = (I "foo"); params = [];
          variants =
          [{ id = (I "A");
             arg = (Some (Con ((I "list"), [(Con ((I "int"), []))]))) }
            ]
          })
      ]
    |}]

let%expect_test _ =
  run {| type foo = A of (int, string) map |} ;
  [%expect
    {|
    [(Type
        { id = (I "foo"); params = [];
          variants =
          [{ id = (I "A");
             arg =
             (Some (Con ((I "map"),
                      [(Con ((I "int"), [])); (Con ((I "string"), []))])))
             }
            ]
          })
      ]
    |}]

let%expect_test _ =
  run {| type foo = A of 'a -> 'b -> 'c |} ;
  [%expect
    {|
    [(Type
        { id = (I "foo"); params = [];
          variants =
          [{ id = (I "A");
             arg =
             (Some (Arr ((Var (I "a")), (Arr ((Var (I "b")), (Var (I "c"))))))) }
            ]
          })
      ]
    |}]

let%expect_test _ =
  run {| type foo = A of 'a * 'b * 'c |} ;
  [%expect
    {|
    [(Type
        { id = (I "foo"); params = [];
          variants =
          [{ id = (I "A");
             arg = (Some (Tuple ((Var (I "a")), (Var (I "b")), [(Var (I "c"))])))
             }
            ]
          })
      ]
    |}]

let%expect_test _ =
  run {| type foo = A of 'some_type_var |} ;
  [%expect
    {|
    [(Type
        { id = (I "foo"); params = [];
          variants = [{ id = (I "A"); arg = (Some (Var (I "some_type_var"))) }] })
      ]
    |}]

let%expect_test _ =
  run
    {| type foo = A of 
         ('a -> int * (string, unit, 'b -> 'c) foo bar option) -> e |} ;
  [%expect
    {|
    [(Type
        { id = (I "foo"); params = [];
          variants =
          [{ id = (I "A");
             arg =
             (Some (Arr (
                      (Arr ((Var (I "a")),
                         (Tuple
                            ((Con ((I "int"), [])),
                             (Con ((I "option"),
                                [(Con ((I "bar"),
                                    [(Con ((I "foo"),
                                        [(Con ((I "string"), []));
                                          (Con ((I "unit"), []));
                                          (Arr ((Var (I "b")), (Var (I "c"))))]
                                        ))
                                      ]
                                    ))
                                  ]
                                )),
                             []))
                         )),
                      (Con ((I "e"), [])))))
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
    [(Let (Nonrec,
        ({ pat = (Tuple ((Var (I "f")), (Var (I "s")), []));
           expr =
           (Tuple
              ((Apply ((Apply ((Id (I "+")), (Id (I "f")))), (Id (I "s")))),
               (Apply ((Apply ((Id (I "-")), (Id (I "f")))), (Id (I "s")))),
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
    [(Let (Nonrec,
        ({ pat = (Var (I ">>="));
           expr =
           (Fun (((Var (I "a")), [(Var (I "b"))]),
              (Apply ((Apply ((Id (I "**")), (Id (I "a")))), (Id (I "b"))))))
           },
         [])
        ))
      ]
    |}]

let%expect_test _ =
  run {| let (++) a b = a + b |} ;
  [%expect
    {|
    [(Let (Nonrec,
        ({ pat = (Var (I "++"));
           expr =
           (Fun (((Var (I "a")), [(Var (I "b"))]),
              (Apply ((Apply ((Id (I "+")), (Id (I "a")))), (Id (I "b"))))))
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
    [(Let (Rec,
        ({ pat = (Var (I "f"));
           expr =
           (Fun (((Var (I "n")), []),
              (Apply ((Apply ((Id (I "+")), (Id (I "n")))), (Const (Int 1))))))
           },
         [])
        ))
      ]
    |}]

let%expect_test _ =
  run {| letrec f n = n + 1 |} ;
  [%expect
    {|
    [(Eval
        (Apply (
           (Apply ((Id (I "=")),
              (Apply ((Apply ((Id (I "letrec")), (Id (I "f")))), (Id (I "n")))))),
           (Apply ((Apply ((Id (I "+")), (Id (I "n")))), (Const (Int 1)))))))
      ]
    |}]

let%expect_test _ =
  run {| let reca = 1 |} ;
  [%expect
    {| [(Let (Nonrec, ({ pat = (Var (I "reca")); expr = (Const (Int 1)) }, [])))] |}]

let%expect_test _ =
  run {| type 'a list = Nil | Cons of 'a * 'a list |} ;
  [%expect
    {|
    [(Type
        { id = (I "list"); params = [(I "a")];
          variants =
          [{ id = (I "Nil"); arg = None };
            { id = (I "Cons");
              arg =
              (Some (Tuple
                       ((Var (I "a")), (Con ((I "list"), [(Var (I "a"))])), [])))
              }
            ]
          })
      ]
    |}]

let%expect_test _ = run {| 1a |} ; [%expect {| syntax error |}]

let%expect_test _ =
  run {| 1 ;; a |} ;
  [%expect {| [(Eval (Const (Int 1))); (Eval (Id (I "a")))] |}]
