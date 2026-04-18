open Pcf.Pp_term
open Pcf.Term
open Alcotest

let pp_to_string pp x =
  let buf = Buffer.create 64 in
  let fmt = Format.formatter_of_buffer buf in
  pp fmt x;
  Format.pp_print_flush fmt ();
  Buffer.contents buf

let test_int () =
  let t = INT 42 in
  check string "int" "42" (pp_to_string pp_term t)

let test_var () =
  let t = VAR "x" in
  check string "var" "x" (pp_to_string pp_term t)

let test_fun () =
  let t = FUN ("x", INT 1) in
  check string "fun" "fun x -> 1" (pp_to_string pp_term t)

let test_app () =
  let t = APP (VAR "f", INT 2) in
  check string "app" "(f 2)" (pp_to_string pp_term t)

let test_bop () =
  let t = BOP (INT 1, ADD, INT 2) in
  check string "bop" "(1 + 2)" (pp_to_string pp_term t)

let test_ifz () =
  let t = IFZ (INT 0, INT 1, INT 2) in
  check string "ifz" "if 0 = 0 then 1 else 2" (pp_to_string pp_term t)

let test_let () =
  let t = LET ("x", INT 1, VAR "x") in
  check string "let" "let x = 1 in x" (pp_to_string pp_term t)

let test_fix () =
  let t = FIX ("x", VAR "x") in
  check string "fix" "fix x. x" (pp_to_string pp_term t)

let () =
  run "pp tests"
    [
      ( "pp",
        [
          test_case "int" `Quick test_int;
          test_case "var" `Quick test_var;
          test_case "fun" `Quick test_fun;
          test_case "app" `Quick test_app;
          test_case "bop" `Quick test_bop;
          test_case "ifz" `Quick test_ifz;
          test_case "let" `Quick test_let;
          test_case "fix" `Quick test_fix;
        ] );
    ]
