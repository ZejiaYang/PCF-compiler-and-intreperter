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
