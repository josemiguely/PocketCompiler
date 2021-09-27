(** Parser **)
open Ast
open Printf
open CCSexp

let rec parse_exp (sexp : sexp) : expr =
  match sexp with
  | `Atom "true" -> Bool true
  | `Atom "false" -> Bool false
  | `Atom s -> (
    match Int64.of_string_opt s with Some n -> Num n | None -> Id s )
  | `List (`Atom "@sys" :: `Atom name :: args) -> (
    match args with
    | [] -> failwith (sprintf "Invalid foreign function call: %s" (to_string sexp))
    | _ -> ApplyFF (name, List.map parse_exp args) )
  | `List [eop; e] -> (
    match eop with 
    | `Atom "add1" -> Prim1 (Add1, parse_exp e)
    | `Atom "sub1" -> Prim1 (Sub1, parse_exp e)
    | `Atom "print" -> Prim1 (Print, parse_exp e)
    | `Atom fo_name -> ApplyFO (fo_name, [parse_exp e])
    | _ -> failwith (sprintf "Not a valid expr: %s" (to_string sexp))
    )
  | `List [eop; e1; e2] -> (
    match eop with
    | `Atom "let" -> (
      match e1 with
      | `List [`Atom id; e] -> Let (id, parse_exp e, parse_exp e2)
      | _ -> failwith "parse error in let" )
    | `Atom "+" -> Prim2 (Add, parse_exp e1, parse_exp e2)
    | `Atom "and" -> Prim2 (And, parse_exp e1, parse_exp e2)
    | `Atom "<=" -> Prim2 (Lte, parse_exp e1, parse_exp e2)
    | `Atom fo_name -> ApplyFO (fo_name, [parse_exp e1 ; parse_exp e2])
    | _ -> failwith (sprintf "Not a valid expr: %s" (to_string sexp))
    )
  | `List [`Atom "if"; e1; e2; e3] -> If (parse_exp e1, parse_exp e2, parse_exp e3)
  | `List (`Atom fo_name :: e2) -> ApplyFO (fo_name, List.map parse_exp e2)
  | _ -> failwith (sprintf "Not a valid expr: %s" (to_string sexp))

let rec parse_prog (sexp : sexp) : prog =
  match sexp with
  | `List (hd :: tl) -> (
    match hd with
    | `List [`Atom "def" ; `List (`Atom name :: args) ; body] ->
      let (fundefs, expr) = parse_prog (`List tl) in
      let arg_names = List.map parse_arg_name args in
      [ Native (name, arg_names, parse_exp body) ] @ fundefs, expr
    | `List (`Atom "defsys" :: `Atom name :: arg_spec) -> (
      match List.rev arg_spec with
      | (ret :: `Atom "->" :: args) -> 
        let (fundefs, expr) = parse_prog (`List tl) in
        let arg_types = List.map parse_c_type (List.rev args) in
        let ret_type = parse_c_type ret in
        [ Foreign (name, arg_types, ret_type) ] @ fundefs, expr
      | _ -> failwith (sprintf "Not a valid type declaration: %s" (to_string (`List arg_spec)))
    )
    | _ -> [], parse_exp sexp
  )
  | _ -> [], parse_exp sexp

and parse_arg_name (sexp : sexp) : string =
  match sexp with
  | `Atom name -> name
  | _ -> failwith (sprintf "Not a valid argument name: %s" (to_string sexp))

and parse_c_type (sexp : sexp) : ctype =
  match sexp with
  | `Atom "any" -> CAny
  | `Atom "int" -> CInt
  | `Atom "bool" -> CBool
  | _ -> failwith (sprintf "Not a valid type declaration: %s" (to_string sexp))

let sexp_from_file : string -> CCSexp.sexp =
 fun filename ->
  match CCSexp.parse_file filename with
  | Ok s -> s
  | Error msg -> failwith (sprintf "Unable to parse file %s: %s" filename msg)

let sexp_from_string (src : string) : CCSexp.sexp =
  match CCSexp.parse_string src with
  | Ok s -> s
  | Error msg -> failwith (sprintf "Unable to parse src %s: %s" src msg)
