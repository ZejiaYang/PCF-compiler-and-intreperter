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
    {
      name = "cbn_list_lazy_head";
      (* This passes only if the tail is not evaluated! *)
      term = HD (CONS (INT 10, diverge));
      expected = VINT 10;
    };
    {
      name = "cbn_list_lazy_tail";
      (* This passes only if the head is not evaluated! *)
      term = IFNIL (TL (CONS (diverge, NIL)), INT 1, INT 0);
      expected = VINT 1;
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

let list_tests =
  [
    {
      name = "list_head_extraction";
      term = HD (CONS (INT 10, NIL));
      expected = VINT 10;
    };
    {
      name = "list_tail_is_nil";
      term = IFNIL (TL (CONS (INT 10, NIL)), INT 1, INT 0);
      expected = VINT 1;
    };
    {
      name = "list_nested_head";
      (* HEAD (TAIL (CONS (10, CONS (20, NIL)))) ==> 20 *)
      term = HD (TL (CONS (INT 10, CONS (INT 20, NIL))));
      expected = VINT 20;
    };
    {
      name = "list_complex_compute";
      (* HEAD (CONS (1 + 2, NIL)) ==> 3 *)
      term = HD (CONS (BOP (INT 1, ADD, INT 2), NIL));
      expected = VINT 3;
    };
    (* {
      name = "list";
      (* HEAD (CONS (1 + 2, NIL)) ==> 3 *)
      term = CONS (BOP (INT 1, ADD, INT 2), NIL);
      expected = VCONS (VINT 3, VNIL);
    }; *)
  ]
