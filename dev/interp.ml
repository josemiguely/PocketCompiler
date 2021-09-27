(** Interpreter **)
open Ast

(** Values **)
type value = 
  NumV of int64 |
  BoolV of bool

(** Declarations **)
type native_fun = NativeFunction of string list * expr
type foreign_fun = ForeignFunction of ctype list * ctype * (value list -> value)

(* Pretty printing *)
let string_of_val(v : value) : string =
  match v with
  | NumV n -> Int64.to_string n
  | BoolV b -> if b then "true" else "false"

let string_of_nf(_: native_fun) : string = "< Native Function >"

let string_of_ff(_: foreign_fun) : string = "< Foreign Function >"

(** Testing Foreign Functions **)
let foreign_prelude = [
  "print", 1, (
    fun vls -> 
      match vls with 
      | v :: [] -> Printf.printf "%s" (string_of_val v) ; v
      | _ -> failwith "Runtime error: Print expected 1 argument")
  ]

  (* Lifting functions on OCaml primitive types to operate on language values *)
let liftIII : (int64 -> int64 -> int64) -> value -> value -> value =
  fun op e1 e2 ->
    match e1, e2 with
    | NumV n1, NumV n2 -> NumV (op n1 n2)
    | _ -> failwith "runtime type error"

let liftBBB : (bool -> bool -> bool) -> value -> value -> value =
  fun op e1 e2 ->
    match e1, e2 with
    | BoolV b1, BoolV b2 -> BoolV (op b1 b2)
    | _ -> failwith "runtime type error"    

let liftIIB : (int64 -> int64 -> bool) -> value -> value -> value =
  fun op e1 e2 ->
    match e1, e2 with
    | NumV n1, NumV n2 -> BoolV (op n1 n2)
    | _ -> failwith "runtime type error"

(* Lexic Environment *)
type vars = (string * value) list
let empty_vars : vars = []

(* Native Function Environment *)
type nfs = (string * native_fun) list
let empty_nfs : nfs = []

(* Foreign Environment *)
type ffs = (string * foreign_fun) list
let empty_ffs : ffs = []

(* Environment *)
type env = nfs * ffs * vars
let empty_env : env = empty_nfs, empty_ffs, empty_vars 

(* Extend *)
let extend_var : string -> value -> env -> env =
  fun x v (nfs, ffs, vars) ->
    nfs, ffs, (x, v) :: vars
let extend_nfun : string -> native_fun -> env -> env =
  fun x nf (nfs, ffs, vars) ->
    (x, nf) :: nfs, ffs, vars
let extend_ffun : string -> foreign_fun -> env -> env =
  fun x ff (nfs, ffs, vars) ->
    nfs, (x, ff) :: ffs, vars
let concat_env : env -> env -> env =
  fun (nfs1, ffs1, vars1) (nfs2, ffs2, vars2) ->
    nfs1 @ nfs2, ffs1 @ ffs2, vars1 @ vars2

let dynamic_typecheck =
  fun (v, t) ->
    match v, t with
    | NumV _, CInt | BoolV _, CBool | _, CAny -> v
    | NumV _, CBool -> failwith (Printf.sprintf "Runtime type error: Expected Bool but got Int %s" (string_of_val v))
    | BoolV _, CInt -> failwith (Printf.sprintf "Runtime type error: Expected Int but got Bool %s" (string_of_val v))

(* Lookup *)
let lookup_var : string -> env -> value =
  fun x (_, _, vars) ->
    List.assoc x vars
let lookup_nf : string -> env -> native_fun =
  fun x (nfs, _, _) ->
    List.assoc x nfs
let lookup_ff : string -> env -> foreign_fun =
  fun x (_, ffs, _) ->
    List.assoc x ffs

(* interpreter *)
let rec interp expr env =
  match expr with
  | Id x -> lookup_var x env
  | Num n -> NumV n
  | Bool b -> BoolV b
  | Prim1 (op, e) -> (
    match op with
    | Add1 -> liftIII ( Int64.add ) (interp e env) (NumV 1L)
    | Sub1 -> liftIII ( Int64.sub ) (interp e env) (NumV 1L)
    | Print -> (interp (ApplyFF ("print", [e])) env) )
  | Prim2 (op, e1, e2) -> 
    (match op with
    | Add -> liftIII ( Int64.add ) 
    | And -> liftBBB ( && ) 
    | Lte -> liftIIB ( <= )) (interp e1 env) (interp e2 env)
  | Let (x, e , b) -> interp b (extend_var x (interp e env) env)
  | If (e1, e2, e3) -> 
    (match (interp e1 env) with
    | BoolV b -> if b then interp e2 env else interp e3 env
    | _ -> failwith "runtime type error")
  | ApplyFO (name, ael) -> 
    let NativeFunction (params, body) = lookup_nf name env in
    let vals = List.map (fun e -> interp e env) ael in
    let param_vals = List.combine params vals in
    let env = List.fold_left (fun env (n, v) -> extend_var n v env) env param_vals in
    interp body env
  | ApplyFF (name, ael) -> 
    let ForeignFunction (arg_types, ret_type, f) = lookup_ff name env in
    let val_types = List.combine (List.map (fun e -> interp e env) ael) arg_types in
    let vals = List.map dynamic_typecheck val_types in
    let result = f vals in
    dynamic_typecheck (result, ret_type)
      

let interp_defs (defs : fundef list) : env =
  List.fold_left (
    fun env def ->
      match def with
      | Native (name, params, body) ->
        extend_nfun name (NativeFunction (params, body)) env
      | Foreign (name, arg_types, ret_type) -> (
        let arity = (List.length arg_types) in
        let cls = List.find_opt (fun (n, a, _) -> (String.equal n name) && (arity == a)) foreign_prelude in
        match cls with
        | Some (_, _, f) -> extend_ffun name (ForeignFunction (arg_types, ret_type, f)) env
        | None -> failwith (Printf.sprintf "Compile error: Foreign Function %s of arity %d doesn't exist" name (List.length arg_types) ) )
    ) empty_env defs

let interp_prog prog env =
  let defs, expr = prog in
  let env = concat_env (interp_defs defs) env in
  interp expr env
