open Db_term
open Pp_term

type dbinterpreter = dbterm * dbenv -> dbvalue

let rec find (i : int) = function
  | DBEND -> failwith ("Unbound var pos" ^ string_of_int i)
  | DBNEXT (t, e) -> if i = 0 then t else find (i - 1) e

let rec dbinterp_by_name : dbinterpreter =
 fun (p, e) ->
  match p with
  | DBVAR i -> (
      match find i e with
      | DBTHUNK (t, e) -> dbinterp_by_name (t, e)
      | _ -> find i e)
  | DBINT n -> VDBINT n
  | DBFUN t -> VDBFUN (t, e)
  | DBBOP (p1, op, p2) -> (
      let v1 = dbinterp_by_name (p1, e) in
      let v2 = dbinterp_by_name (p2, e) in
      match (v1, op, v2) with
      | VDBINT n1, ADD, VDBINT n2 -> VDBINT (n1 + n2)
      | VDBINT n1, MINUS, VDBINT n2 -> VDBINT (max (n1 - n2) 0)
      | VDBINT n1, MULTI, VDBINT n2 -> VDBINT (n1 * n2)
      | VDBINT n1, DIVIDE, VDBINT 0 -> failwith "divide by zero"
      | VDBINT n1, DIVIDE, VDBINT n2 -> VDBINT (n1 / n2)
      | _ -> failwith "binary operands not integer")
  | DBIFZ (p1, p2, p3) -> (
      match dbinterp_by_name (p1, e) with
      | VDBINT n ->
          if n = 0 then dbinterp_by_name (p2, e) else dbinterp_by_name (p3, e)
      | _ -> failwith "if condition not bool")
  | DBAPP (p1, p2) -> (
      match dbinterp_by_name (p1, e) with
      | VDBFUN (t, e1) -> dbinterp_by_name (t, DBNEXT (DBTHUNK (p2, e), e1))
      | _ -> failwith "not a function in application")
  | DBLET (p1, p2) -> dbinterp_by_name (DBAPP (DBFUN p2, p1), e)
  | DBFIXFUN p1 -> dbinterp_by_name (DBFUN p1, DBNEXT (DBTHUNK (p, e), e))
  (* -- Pair -- *)
  | DBPAIR (p1, p2) -> DBTHUNK (p, e)
  | DBFST p -> (
      match dbinterp_by_name (p, e) with
      | DBTHUNK (DBPAIR (p1, _), e) -> dbinterp_by_name (p1, e)
      | _ -> failwith "fst not pair")
  | DBSND p -> (
      match dbinterp_by_name (p, e) with
      | DBTHUNK (DBPAIR (_, p2), e) -> dbinterp_by_name (p2, e)
      | _ -> failwith "snd not pair")
  (* -- List -- *)
  | DBNIL -> VDBNIL
  | DBCONS (t, l) ->
      VDBCONS (DBTHUNK (t, e), DBTHUNK (l, e))
      (*call-by-name, lazy evaluation for list*)
  | DBIFNIL (y, p1, p2) -> (
      match dbinterp_by_name (y, e) with
      | VDBNIL -> dbinterp_by_name (p1, e)
      | VDBCONS _ -> dbinterp_by_name (p2, e)
      | _ -> failwith "ifnil condition not list")
  | DBHD p -> (
      match dbinterp_by_name (p, e) with
      | VDBNIL -> failwith "empty list"
      | VDBCONS (DBTHUNK (t, e), l) -> dbinterp_by_name (t, e)
      | _ -> failwith (Format.asprintf "impossible construct %a" pp_db_term p))
  | DBTL p -> (
      match dbinterp_by_name (p, e) with
      | VDBNIL -> failwith "empty list"
      | VDBCONS (_, DBTHUNK (l, e)) -> dbinterp_by_name (l, e)
      | _ -> failwith (Format.asprintf "impossible construct %a" pp_db_term p))
  (* -- Tree -- *)
  | DBLEAF p -> VDBLEAF (DBTHUNK (p, e))
  | DBTREE (l, u) -> VDBTREE (DBTHUNK (l, e), DBTHUNK (u, e))
  | DBITEM p -> (
      match dbinterp_by_name (p, e) with
      | VDBLEAF (DBTHUNK (p, e)) -> dbinterp_by_name (p, e)
      | _ -> failwith (Format.asprintf "impossible construct %a" pp_db_term p))
  | DBIFLEAF (p, p1, p2) -> (
      match dbinterp_by_name (p, e) with
      | VDBLEAF _ -> dbinterp_by_name (p1, e)
      | VDBTREE _ -> dbinterp_by_name (p2, e)
      | _ -> failwith (Format.asprintf "impossible construct %a" pp_db_term p))
  | DBLTREE p -> (
      match dbinterp_by_name (p, e) with
      | VDBTREE (DBTHUNK (p, e), _) -> dbinterp_by_name (p, e)
      | VDBLEAF _ -> failwith "LTREE a leaf"
      | _ -> failwith (Format.asprintf "impossible construct %a" pp_db_term p))
  | DBRTREE p -> (
      match dbinterp_by_name (p, e) with
      | VDBTREE (_, DBTHUNK (p, e)) -> dbinterp_by_name (p, e)
      | VDBLEAF _ -> failwith "RTREE a leaf"
      | _ -> failwith (Format.asprintf "impossible construct %a" pp_db_term p))

let rec dbinterp_by_value : dbinterpreter =
 fun (p, e) ->
  match p with
  | DBVAR i -> find i e
  | DBINT n -> VDBINT n
  | DBFUN t -> VDBFUN (t, e)
  | DBBOP (p1, op, p2) -> (
      let v1 = dbinterp_by_value (p1, e) in
      let v2 = dbinterp_by_value (p2, e) in
      match (v1, op, v2) with
      | VDBINT n1, ADD, VDBINT n2 -> VDBINT (n1 + n2)
      | VDBINT n1, MINUS, VDBINT n2 -> VDBINT (max (n1 - n2) 0)
      | VDBINT n1, MULTI, VDBINT n2 -> VDBINT (n1 * n2)
      | VDBINT n1, DIVIDE, VDBINT 0 -> failwith "divide by zero"
      | VDBINT n1, DIVIDE, VDBINT n2 -> VDBINT (n1 / n2)
      | _ -> failwith "binary operands not integer")
  | DBIFZ (p1, p2, p3) -> (
      match dbinterp_by_value (p1, e) with
      | VDBINT n ->
          if n = 0 then dbinterp_by_value (p2, e) else dbinterp_by_value (p3, e)
      | _ -> failwith "if condition not bool")
  | DBAPP (p1, p2) -> (
      match dbinterp_by_value (p1, e) with
      | VDBFUN (t, e1) ->
          dbinterp_by_value (t, DBNEXT (dbinterp_by_value (p2, e), e1))
      | VDBFIXFUN (t, e1) ->
          dbinterp_by_value
            ( t,
              DBNEXT (dbinterp_by_value (p2, e), DBNEXT (VDBFIXFUN (t, e1), e1))
            )
      | _ -> failwith "not a function in application")
  | DBLET (p1, p2) -> dbinterp_by_value (DBAPP (DBFUN p2, p1), e)
  | DBFIXFUN p1 -> VDBFIXFUN (p1, e)
  (* -- Pair -- *)
  | DBPAIR (p1, p2) ->
      VDBPAIR (dbinterp_by_value (p1, e), dbinterp_by_value (p2, e))
  | DBFST p -> (
      match dbinterp_by_value (p, e) with
      | VDBPAIR (v1, _) -> v1
      | _ -> failwith "fst not pair")
  | DBSND p -> (
      match dbinterp_by_value (p, e) with
      | VDBPAIR (_, v2) -> v2
      | _ -> failwith "snd not pair")
  (* -- List -- *)
  | DBNIL -> VDBNIL
  | DBCONS (t, l) -> VDBCONS (dbinterp_by_value (t, e), dbinterp_by_value (l, e))
  | DBIFNIL (y, p1, p2) -> (
      match dbinterp_by_value (y, e) with
      | VDBNIL -> dbinterp_by_value (p1, e)
      | VDBCONS _ -> dbinterp_by_value (p2, e)
      | _ -> failwith "ifnil condition not list")
  | DBHD t -> (
      match dbinterp_by_value (t, e) with
      | VDBNIL -> failwith "empty list"
      | VDBCONS (v, l) -> v
      | _ -> failwith (Format.asprintf "impossible construct %a" pp_db_term p))
  | DBTL t -> (
      match dbinterp_by_value (t, e) with
      | VDBNIL -> failwith "empty list"
      | VDBCONS (v, l) -> l
      | _ -> failwith (Format.asprintf "impossible construct %a" pp_db_term p))
  (* -- Tree -- *)
  | DBLEAF n -> VDBLEAF (dbinterp_by_value (n, e))
  | DBTREE (l, r) -> VDBTREE (dbinterp_by_value (l, e), dbinterp_by_value (r, e))
  | DBITEM p -> (
      match dbinterp_by_value (p, e) with
      | VDBLEAF n -> n
      | _ -> failwith (Format.asprintf "impossible construct %a" pp_db_term p))
  | DBIFLEAF (p, p1, p2) -> (
      match dbinterp_by_value (p, e) with
      | VDBLEAF n -> dbinterp_by_value (p1, e)
      | VDBTREE _ -> dbinterp_by_value (p2, e)
      | _ -> failwith "IFLEAF not tree")
  | DBLTREE p -> (
      match dbinterp_by_value (p, e) with
      | VDBTREE (l, _) -> l
      | VDBLEAF _ -> failwith "LTREE a leaf"
      | _ -> failwith (Format.asprintf "impossible construct %a" pp_db_term p))
  | DBRTREE p -> (
      match dbinterp_by_value (p, e) with
      | VDBTREE (_, r) -> r
      | VDBLEAF _ -> failwith "RTREE a leaf"
      | _ -> failwith (Format.asprintf "impossible construct %a" pp_db_term p))
