type prim1 =
  | Add1
  | Sub1
  | Print
  | IsNum
  | IsBool
  | IsPair
  | Fst
  | Snd
  | Input

type prim2 =
  | Plus
  | Minus
  | Times
  | Less
  | Greater
  | Equal
  | SetFst
  | SetSnd

type expr =
  | ELet of (string * expr) list * expr
  | ESeq of expr list
  | EPrim1 of prim1 * expr
  | EPrim2 of prim2 * expr * expr
  | EApp of string * expr list
  | EPair of expr * expr
  | EIf of expr * expr * expr
  | ENumber of int
  | EBool of bool
  | EId of string

type type_tag =
  | TBool
  | TNum
  | TPair

type decl =
  | DFun of string * string list * expr

type program =
  | Program of decl list * expr

