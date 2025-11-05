open Ast

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  Parser.prog Lexer.read lexbuf

let unbound_var_err = "Unbound variable"
let bop_err = "Operator and operand type mismatch"
let if_guard_err = "Guard of conditional must be boolean"
let annotation_err = "Type annotation mismatch"

module StrMap = Map.Make(String)

let round2 (f: float) : float =
  floor (f *. 100. +. 0.5) /. 100.

let rec typeof (tenv : typ StrMap.t) (e : expr) : typ =
  match e with
  | Var x ->
      (try StrMap.find x tenv with Not_found -> failwith unbound_var_err)
  | Int _ -> TInt
  | Bool _ -> TBool
  | Float _ -> TFloat
  | Let (x, t, e1, e2) ->
      let t1 = typeof tenv e1 in
      if t1 <> t then failwith annotation_err;
      let tenv' = StrMap.add x t tenv in
      typeof tenv' e2
  | If (e1, e2, e3) ->
      begin match typeof tenv e1 with
      | TBool ->
          let t2 = typeof tenv e2 in
          let t3 = typeof tenv e3 in
          if t2 = t3 then t2 else failwith bop_err
      | _ -> failwith if_guard_err
      end
  | Binop (b, e1, e2) ->
      begin match b with
      | Add | Sub | Mult | Div ->
          if typeof tenv e1 = TInt && typeof tenv e2 = TInt then TInt
          else failwith bop_err
      | FAdd | FSub | FMult | FDiv ->
          if typeof tenv e1 = TFloat && typeof tenv e2 = TFloat then TFloat
          else failwith bop_err
      | Leq | Geq ->
          begin match typeof tenv e1, typeof tenv e2 with
          | TInt, TInt -> TBool
          | TFloat, TFloat -> TBool
          | _ -> failwith bop_err
          end
      end

let typeof_top e = typeof StrMap.empty e

let rec eval (env : expr StrMap.t) (e : expr) : expr =
  match e with
  | Var x ->
      (try StrMap.find x env with Not_found -> failwith unbound_var_err)
  | Int _ | Bool _ | Float _ -> e
  | Let (x, _, e1, e2) ->
      let v1 = eval env e1 in
      let env' = StrMap.add x v1 env in
      eval env' e2
  | If (e1, e2, e3) ->
      begin match eval env e1 with
      | Bool true -> eval env e2
      | Bool false -> eval env e3
      | _ -> failwith if_guard_err
      end
  | Binop (b, e1, e2) ->
      begin match b, eval env e1, eval env e2 with
      | Add, Int n1, Int n2 -> Int (n1 + n2)
      | Sub, Int n1, Int n2 -> Int (n1 - n2)
      | Mult, Int n1, Int n2 -> Int (n1 * n2)
      | Div, Int n1, Int n2 -> Int (n1 / n2)
      | Leq, Int n1, Int n2 -> Bool (n1 <= n2)
      | Geq, Int n1, Int n2 -> Bool (n1 >= n2)
      | FAdd, Float f1, Float f2 -> Float (round2 (f1 +. f2))
      | FSub, Float f1, Float f2 -> Float (round2 (f1 -. f2))
      | FMult, Float f1, Float f2 -> Float (round2 (f1 *. f2))
      | FDiv, Float f1, Float f2 -> Float (round2 (f1 /. f2))
      | Leq, Float f1, Float f2 -> Bool (f1 <= f2)
      | Geq, Float f1, Float f2 -> Bool (f1 >= f2)
      | _ -> failwith bop_err
      end

let eval_top e = eval StrMap.empty e

let interp (s : string) : expr =
  let e = parse s in
  ignore (typeof_top e);
  eval_top e
