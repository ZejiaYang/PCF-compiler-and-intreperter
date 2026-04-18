open Term
open Format

let pp_op fmt = function
  | ADD -> fprintf fmt "+"
  | MINUS -> fprintf fmt "-"
  | MULTI -> fprintf fmt "*"
  | DIVIDE -> fprintf fmt "/"

let rec pp_term fmt t =
  match t with
  | INT n -> fprintf fmt "%d" n
  | VAR x -> fprintf fmt "%s" x
  | FUN (x, p) -> fprintf fmt "@[<2>fun %s ->@ %a@]" x pp_term p
  | APP (p1, p2) -> fprintf fmt "@[<2>(%a %a)@]" pp_term p1 pp_term p2
  | BOP (p1, op, p2) ->
      fprintf fmt "@[<2>(%a %a %a)@]" pp_term p1 pp_op op pp_term p2
  | IFZ (p1, p2, p3) ->
      fprintf fmt "@[<2>if %a = 0 then@ %a@ else@ %a@]" pp_term p1 pp_term p2
        pp_term p3
  | FIX (f, p) -> fprintf fmt "@[<2>fix %s.@ %a@]" f pp_term p
  | LET (x, p1, p2) ->
      fprintf fmt "@[<2>let %s = %a in@ %a@]" x pp_term p1 pp_term p2
