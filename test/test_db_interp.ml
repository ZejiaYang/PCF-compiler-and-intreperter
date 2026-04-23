open Pcf.Db_term
open Pcf.Db_interp
open Pcf.Pp_term
open Pcf.Term
open Test_suite
open Alcotest

(* Setup the testable for De Bruijn terms *)
let db_value = testable pp_db_value ( = )

let make_db_suite suite_name (interp : dbinterpreter)
    (tests : abstract_test list) =
  let bundle =
    List.map
      (fun test ->
        (* 1. Translate the expected value and the term *)
        let expected = translate_db_val test.expected VEND in
        let db_term = translate_db test.term VEND in

        ( test.name,
          `Quick,
          fun () ->
            (* --- DEBUG SECTION --- *)
            Format.printf "@.--- Debugging Test: %s ---@." test.name;
            Format.printf "Original Value: %a@." pp_value test.expected;
            Format.printf "Translated DB Term: %a@." pp_db_term db_term;
            Format.printf "Running interpreter...@.";

            (* ---------------------- *)
            let actual = interp (db_term, DBEND) in

            Format.printf "Result obtained successfully!@.";
            check db_value test.name expected actual ))
      tests
  in
  (suite_name, bundle)

let () =
  run "De Bruijn Interpreter Suite"
    [
      (* make_db_suite "standard_by_name" dbinterp_by_name standard_tests;
      make_db_suite "standard_by_value" dbinterp_by_value standard_tests;
      make_db_suite "pairs_by_value" dbinterp_by_value pair_tests;
      make_db_suite "pairs_by_name" dbinterp_by_name pair_tests;
      make_db_suite "cbn_test" dbinterp_by_name cbn_tests;
      make_db_suite "list_by_name" dbinterp_by_name list_tests;
      make_db_suite "list_by_value_recur" dbinterp_by_value list_tests; *)
      make_db_suite "tree_test_by_value" dbinterp_by_value tree_tests;
      make_db_suite "tree_test_by_name" dbinterp_by_name tree_tests;
    ]
