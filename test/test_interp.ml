open Pcf.Interp
open Pcf.Pp_term
open Pcf.Term
open Test_suite
open Alcotest

let value = testable pp_value ( = )

let make_suite suite_name (interp : interpreter) (tests : abstract_test list) =
  let bundle =
    List.map
      (fun test ->
        ( test.name,
          `Quick,
          fun () ->
            check value test.name test.expected (interp (test.term, END)) ))
      tests
  in
  (suite_name, bundle)

let () =
  run "interp suite"
    [
      make_suite "standard_by_name" interp_by_name standard_tests;
      make_suite "standard_by_value" interp_by_value standard_tests;
      make_suite "standard_by_value_recur" interp_by_value_recur standard_tests;
      make_suite "pairs_by_value" interp_by_value pair_tests;
      make_suite "pairs_by_value_recur" interp_by_value_recur pair_tests;
      make_suite "pairs_by_name" interp_by_name pair_tests;
      make_suite "cbn_test" interp_by_name cbn_tests;
      make_suite "list_by_name" interp_by_name list_tests;
      make_suite "list_by_value_recur" interp_by_value_recur list_tests;
      make_suite "sorting_tests_by_value" interp_by_value_recur
        sorting_tests_value;
      make_suite "sorting_tests_by_name" interp_by_name sorting_tests_name;
    ]
