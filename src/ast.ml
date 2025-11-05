type bop =
  | Add
  | Sub
  | Mult
  | Div
  | Leq
  | Geq
  | FAdd
  | FSub
  | FMult
  | FDiv

type typ =
  | TInt
  | TBool
  | TFloat

type expr =
  | Var of string
  | Int of int
  | Bool of bool
  | Float of float
  | Binop of bop * expr * expr
  | Let of string * typ * expr * expr
  | If of expr * expr * expr
