(** Parser **)
open Ast
open Printf
open CCSexp

(* let rec parse_exp (sexp : sexp) : tag expr =
  match sexp with
  | `Atom "true" -> Bool (true,-1)
  | `Atom "false" -> Bool (false,-1)
  | `Atom s -> (
    match Int64.of_string_opt s with Some n -> Num (n,-1) | None -> Id (s,-1) )
  | `List [eop; e] -> (
    match eop with 
    | `Atom "add1" -> Prim1 (Add1, parse_exp e,-1)
    | `Atom "sub1" -> Prim1 (Sub1, parse_exp e,-1)
    | _ -> failwith (sprintf "Not a valid expr1: %s" (to_string sexp)) )
  | `List [eop; e1; e2] -> (
    match eop with
    | `Atom "let" -> (
      match e1 with
      | `List [`Atom id; e] -> Let (id, parse_exp e, parse_exp e2,-1)
      | _ -> failwith "parse error in let" )
    | `Atom "+" -> Prim2 (Add, parse_exp e1, parse_exp e2,-1)
    | `Atom "and" -> Prim2 (And, parse_exp e1, parse_exp e2,-1)
    | `Atom "<=" -> Prim2 (Lte, parse_exp e1, parse_exp e2,-1)
    | _ -> failwith (sprintf "Not a valid expr2: %s" (to_string sexp)) )
  | `List [`Atom "if"; e1; e2; e3] -> If (parse_exp e1, parse_exp e2, parse_exp e3,-1)
  | _ -> failwith (sprintf "Not a valid expr3: %s" (to_string sexp)) *)

let rec parse_exp (sexp : sexp) : tag expr =
  match sexp with
  | `Atom "true" -> Bool (true,-1)
  | `Atom "false" -> Bool (false,-1)
  | `Atom s -> (
    match Int64.of_string_opt s with Some n -> Num (n,-1) | None -> Id (s,-1) )
  | `List [eop; e] -> (
    match eop with 
    | `Atom "add1" -> Prim1 (Add1, parse_exp e,-1)
    | `Atom "sub1" -> Prim1 (Sub1, parse_exp e,-1)
    | `Atom "not" -> Prim1 (Not, parse_exp e, -1)
    | _ -> failwith (sprintf "Not a valid expr: %s" (to_string sexp)) )
  | `List [eop; e1; e2] -> (
    match eop with
    | `Atom "let" -> (
      match e1 with
      | `List [`Atom id; e] -> Let (id, parse_exp e, parse_exp e2,-1)
      | _ -> failwith "parse error in let" )
    | `Atom "+" -> Prim2 (Add, parse_exp e1, parse_exp e2,-1)
    | `Atom "and" -> Prim2 (And, parse_exp e1, parse_exp e2,-1)
    | `Atom "<=" -> Prim2 (Lte, parse_exp e1, parse_exp e2,-1)
    | `Atom "<" -> Prim2 (Lt, parse_exp e1,parse_exp e2,-1)
    | _ -> failwith (sprintf "Not a valid expr: %s" (to_string sexp)) )
  | `List [`Atom "if"; e1; e2; e3] -> If (parse_exp e1, parse_exp e2, parse_exp e3,-1)
  | _ -> failwith (sprintf "Not a valid expr: %s" (to_string sexp))

let sexp_from_file : string -> CCSexp.sexp =
 fun filename ->
  match CCSexp.parse_file filename with
  | Ok s -> s
  | Error msg -> failwith (sprintf "Unable to parse file %s: %s" filename msg)

let sexp_from_string (src : string) : CCSexp.sexp =
  match CCSexp.parse_string src with
  | Ok s -> s
  | Error msg -> failwith (sprintf "Unable to parse src %s: %s" src msg)
