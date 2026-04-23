open Pcf.Term
open Alcotest

type abstract_test = { name : string; term : term; expected : value }

let pair_tests : abstract_test list =
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

let cbn_tests : abstract_test list =
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
    {
      name = "tree_laziness_ltree";
      (* ITEM (LTREE (TREE (LEAF 7, diverge))) ==> 7 *)
      (* Should pass without hitting the infinite loop in the right branch *)
      term = ITEM (LTREE (TREE (LEAF (INT 7), diverge)));
      expected = VINT 7;
    };
    {
      name = "tree_laziness_rtree";
      (* ITEM (RTREE (TREE (diverge, LEAF 8))) ==> 8 *)
      term = ITEM (RTREE (TREE (diverge, LEAF (INT 8))));
      expected = VINT 8;
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
  ]

(* sorting algorithm tests *)
(* helper insertion logic *)
let insert_logic =
  FIX
    ( "insert",
      FUN
        ( "x",
          FUN
            ( "l",
              IFNIL
                ( VAR "l",
                  CONS (VAR "x", NIL),
                  IFZ
                    ( BOP (VAR "x", MINUS, HD (VAR "l")),
                      CONS (VAR "x", VAR "l"),
                      (* x is smaller or equal, put at front *)
                      CONS
                        ( HD (VAR "l"),
                          APP (APP (VAR "insert", VAR "x"), TL (VAR "l")) ) ) )
            ) ) )

(* Main: sort(list) *)
let sort_logic =
  FIX
    ( "sort",
      FUN
        ( "l",
          IFNIL
            ( VAR "l",
              NIL,
              let head = HD (VAR "l") in
              let sorted_tail = APP (VAR "sort", TL (VAR "l")) in
              APP (APP (insert_logic, head), sorted_tail) ) ) )

let sorting_tests_value =
  [
    {
      name = "insertion_sort_simple";
      term =
        (let list_312 = CONS (INT 3, CONS (INT 1, CONS (INT 2, NIL))) in
         APP (sort_logic, list_312));
      (* Expected: VCONS(1, VCONS(2, VCONS(3, VNIL))) *)
      expected = VCONS (VINT 1, VCONS (VINT 2, VCONS (VINT 3, VNIL)));
    };
    { name = "sort_empty_list"; term = APP (sort_logic, NIL); expected = VNIL };
  ]

let sorting_tests_name =
  let list_312 = CONS (INT 3, CONS (INT 1, CONS (INT 2, NIL))) in
  let sorted_list = APP (sort_logic, list_312) in
  [
    {
      name = "insertion_sort_simple 1st";
      term = HD sorted_list;
      expected = VINT 1;
    };
    {
      name = "insertion_sort_simple 2st";
      term = HD (TL sorted_list);
      expected = VINT 2;
    };
    {
      name = "insertion_sort_simple 3st";
      term = HD (TL (TL sorted_list));
      expected = VINT 3;
    };
    { name = "sort_empty_list"; term = APP (sort_logic, NIL); expected = VNIL };
  ]

let sum_tree_logic =
  FIX
    ( "sum",
      FUN
        ( "t",
          IFLEAF
            ( VAR "t",
              ITEM (VAR "t"),
              BOP
                ( APP (VAR "sum", LTREE (VAR "t")),
                  ADD,
                  APP (VAR "sum", RTREE (VAR "t")) ) ) ) )

let tree_tests =
  [
    {
      name = "leaf_item_extraction";
      (* ITEM (LEAF 42) ==> 42 *)
      term = ITEM (LEAF (INT 42));
      expected = VINT 42;
    };
    {
      name = "tree_left_child_extraction";
      (* ITEM (LTREE (TREE (LEAF 1, LEAF 2))) ==> 1 *)
      term = ITEM (LTREE (TREE (LEAF (INT 1), LEAF (INT 2))));
      expected = VINT 1;
    };
    {
      name = "tree_right_child_extraction";
      (* ITEM (RTREE (TREE (LEAF 1, LEAF 2))) ==> 2 *)
      term = ITEM (RTREE (TREE (LEAF (INT 1), LEAF (INT 2))));
      expected = VINT 2;
    };
    {
      name = "ifleaf_base_case";
      (* ifleaf (LEAF 0) then 1 else 2 ==> 1 *)
      term = IFLEAF (LEAF (INT 0), INT 1, INT 2);
      expected = VINT 1;
    };
    {
      name = "ifleaf_recursive_case";
      (* ifleaf (TREE (LEAF 1, LEAF 2)) then 1 else 2 ==> 2 *)
      term = IFLEAF (TREE (LEAF (INT 1), LEAF (INT 2)), INT 1, INT 2);
      expected = VINT 2;
    };
    {
      name = "nested_tree_navigation";
      (* ITEM (RTREE (LTREE (TREE (TREE (LEAF 0, LEAF 99), LEAF 0)))) ==> 99 *)
      term =
        ITEM
          (RTREE
             (LTREE (TREE (TREE (LEAF (INT 0), LEAF (INT 99)), LEAF (INT 0)))));
      expected = VINT 99;
    };
    {
      name = "recursive_tree_sum";
      (* sum (TREE (LEAF 5, LEAF 10)) ==> 15 *)
      term = APP (sum_tree_logic, TREE (LEAF (INT 5), LEAF (INT 10)));
      expected = VINT 15;
    };
  ]
