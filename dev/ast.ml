(** AST **)
open Printf

(* primitive operators *)
type prim1 = Add1 | Sub1
type prim2 = Add | And | Lte 

(* Algebraic datatype for expressions *)
type expr = 
  | Num of int64 
  | Bool of bool
  | Prim1 of prim1 * expr
  | Prim2 of prim2 * expr * expr
  | Id of string
  | Let of string * expr * expr
  | If of expr * expr * expr
  | Apply of expr * expr list
  | ApplyFF of string * expr list

(* C function argument types *)
type ctype =
  | CAny
  | CInt
  | CBool

(* Function definitions *)
type fundef =
  | Native of string * string list * expr
  | Foreign of string * ctype list * ctype

(* Program including definitions and a body *)
type prog = fundef list * expr

(* Pretty printing expressions - used by testing framework *)
let rec string_of_expr(e : expr) : string = 
  match e with
  | Num n -> Int64.to_string n
  | Bool b -> if b then "true" else "false"
  | Id s -> s
  | Prim1 (op, e) -> sprintf "(%s %s)" 
    (match op with
    | Add1 -> "add1"
    | Sub1 -> "sub1") (string_of_expr e)
  | Prim2 (op, e1, e2) -> sprintf "(%s %s %s)" 
    (match op with 
    | Add -> "+"
    | And -> "and"
    | Lte -> "<=") (string_of_expr e1) (string_of_expr e2)
  | Let (x, e1, e2) -> sprintf "(let (%s %s) %s)" x (string_of_expr e1) (string_of_expr e2) 
  | If (e1, e2, e3) -> sprintf "(if %s %s %s)" (string_of_expr e1) (string_of_expr e2) (string_of_expr e3)
  | Apply (fe, ael) -> sprintf "(%s %s)" (string_of_expr fe) (String.concat " " (List.map string_of_expr ael))
  | ApplyFF (fn, ael) -> sprintf "(@sys %s %s)" fn (String.concat " " (List.map string_of_expr ael))

(* Pretty printing C types - used by testing framework *)
let string_of_ctype(t : ctype) : string =
match t with
| CAny -> "any"
| CInt -> "int"
| CBool -> "bool"

(* Pretty printing function definitions - used by testing framework *)
let string_of_fundef(d : fundef) : string =
  match d with
  | Native (name, arg_ids, body) -> sprintf "(def (%s %s) %s)" name (String.concat " " arg_ids) (string_of_expr body)
  | Foreign (name, arg_types, ret_type) -> sprintf "(defsys %s %s -> %s)" name (String.concat " " (List.map string_of_ctype arg_types)) (string_of_ctype ret_type)

(* Pretty printing a program - used by testing framework *)
let string_of_prog(p : prog) : string =
  let fundefs, body = p in
  String.concat "\n" ((List.map string_of_fundef fundefs) @ [string_of_expr body])
