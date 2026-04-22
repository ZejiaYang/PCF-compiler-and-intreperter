open Pcf.Term
open Alcotest

type abstract_test = { name : string; term : term; expected : value }

let pair_tests =
  [
    {
      name = "pair_fst";
      term = FST (PAIR (INT 10, INT 20));
      expected = VINT 10;
    };
    {
      name = "pair_snd";
      term = SND (PAIR (INT 10, INT 20));
      expected = VINT 20;
    };
    {
      name = "nested_pair";
      term = SND (FST (PAIR (PAIR (INT 1, INT 2), INT 3)));
      expected = VINT 2;
    };
    {
      name = "pair_compute";
      term =
        FST
          (LET
             ( "x",
               INT 5,
               PAIR (BOP (VAR "x", ADD, INT 1), BOP (VAR "x", MINUS, INT 1)) ));
      expected = VINT 6;
    };
  ]

let cbn_tests =
  let diverge = APP (FIX ("f", FUN ("x", APP (VAR "f", VAR "x"))), INT 0) in
  [
    {
      name = "cbn_fun_argument";
      term = APP (FUN ("x", INT 0), diverge);
      expected = VINT 0;
    };
    {
      name = "cbn_fst_only";
      term = FST (PAIR (INT 10, diverge));
      expected = VINT 10;
    };
  ]

let standard_tests : abstract_test list =
  [
    {
      name = "shadowing";
      term = APP (APP (FUN ("x", FUN ("x", VAR "x")), INT 2), INT 3);
      expected = VINT 3;
    };
    {
      name = "higher_order";
      term =
        (let inner = FUN ("x", BOP (VAR "x", ADD, VAR "y")) in
         APP (APP (FUN ("x", FUN ("y", APP (inner, VAR "x"))), INT 4), INT 5));
      expected = VINT 9;
    };
    {
      name = "static_binding";
      term =
        (let f_body = FUN ("y", BOP (VAR "y", ADD, VAR "x")) in
         LET
           ( "x",
             INT 4,
             LET ("f", f_body, LET ("x", INT 5, APP (VAR "f", INT 6))) ));
      expected = VINT 10;
    };
    {
      name = "factorial";
      term =
        (let fact =
           FIX
             ( "f",
               FUN
                 ( "x",
                   IFZ
                     ( VAR "x",
                       INT 1,
                       BOP
                         ( VAR "x",
                           MULTI,
                           APP (VAR "f", BOP (VAR "x", MINUS, INT 1)) ) ) ) )
         in
         APP (fact, INT 3));
      expected = VINT 6;
    };
  ]
