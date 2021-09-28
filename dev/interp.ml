(** Interpreter **)
open Ast

(** Values **)
type value = 
  | NumV of int64
  | BoolV of bool

(** Declarations **)
type func =
  | Fun of string list * expr
  | Sys of ctype list * ctype * (value list -> value)

(* Pretty printing *)
let string_of_val(v : value) : string =
  match v with
  | NumV n -> Int64.to_string n
  | BoolV b -> if b then "true" else "false"

(** Testing Foreign Functions **)
let foreign_func_prelude : (string * int * (value list -> value)) list = [
  "print", 1, (
    fun vls -> 
      match vls with 
      | v :: [] -> Printf.printf "> %s" (string_of_val v) ; v
      | _ -> failwith "Runtime error: Print expected 1 argument"
    ) ;
  (* Next Function as name, arity, *)
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
type env = (string * value) list
let empty_env : env = []
let concat_env : env -> env -> env =
  fun env1 env2 ->
    env1 @ env2
let extend_env : string -> value -> env -> env =
  fun s v env ->
    (s, v) :: env
let lookup_env : string -> env -> value =
  fun s env ->
    List.assoc s env

(* Function Environment *)
type fenv = (string * func) list
let empty_fenv : fenv = []
let concat_fenv : fenv -> fenv -> fenv =
  fun fenv1 fenv2 ->
    fenv1 @ fenv2
let extend_fenv : string -> func -> fenv -> fenv =
  fun s v fenv ->
    (s, v) :: fenv
let lookup_fenv : string -> fenv -> func =
  fun s fenv ->
    List.assoc s fenv

let dynamic_typecheck : value * ctype -> value =
  fun (v, t) ->
    match v, t with
    | NumV _, CInt | BoolV _, CBool | _, CAny -> v
    | NumV _, CBool -> failwith (Printf.sprintf "Runtime type error: Expected Bool but got Int %s" (string_of_val v))
    | BoolV _, CInt -> failwith (Printf.sprintf "Runtime type error: Expected Int but got Bool %s" (string_of_val v))

(* interpreter *)
let rec interp expr env fenv =
  match expr with
  | Id x -> lookup_env x env
  | Num n -> NumV n
  | Bool b -> BoolV b
  | Prim1 (op, e) -> (
    match op with
    | Add1 -> liftIII ( Int64.add ) (interp e env fenv) (NumV 1L)
    | Sub1 -> liftIII ( Int64.sub ) (interp e env fenv) (NumV 1L)
    | Print -> (interp (Apply ("print", [e])) env fenv) )
  | Prim2 (op, e1, e2) -> 
    (match op with
    | Add -> liftIII ( Int64.add ) 
    | And -> liftBBB ( && ) 
    | Lte -> liftIIB ( <= )) (interp e1 env fenv) (interp e2 env fenv)
  | Let (x, e , b) -> interp b (extend_env x (interp e env fenv) env) fenv
  | If (e1, e2, e3) -> 
    (match interp e1 env fenv with
    | BoolV b -> interp (if b then e2 else e3) env fenv
    | _ -> failwith "runtime type error")
  | Apply (name, e_list) -> 
    (match lookup_fenv name fenv with
    | Fun (params, body) -> 
      let vals = List.map (fun e -> interp e env fenv) e_list in
      let param_vals = List.combine params vals in
      let env = List.fold_left (fun env (n, v) -> extend_env n v env) env param_vals in
      interp body env fenv
    | Sys (arg_types, ret_type, lambda) -> 
      let val_types = List.combine (List.map (fun e -> interp e env fenv) e_list) arg_types in
      let vals = List.map dynamic_typecheck val_types in
      let result = lambda vals in
      dynamic_typecheck (result, ret_type) )

let prepare_defs (defs : funcdef list) : fenv =
  List.filter_map (
    fun def ->
      match def with
      | DefFun (name, params, body) -> Some (name, Fun (params, body))
      | DefSys (name, arg_types, ret_type) -> (
        let arity = (List.length arg_types) in
        let cls = List.find_opt (fun (n, a, _) -> (String.equal n name) && (arity == a)) foreign_func_prelude in
        match cls with
        | Some (_, _, lambda) -> Some (name, Sys (arg_types, ret_type, lambda)) 
        | None -> None )
  ) defs

let interp_prog prog env =
  let defs, expr = prog in
  let fenv = concat_fenv (prepare_defs defs) empty_fenv in
  interp expr env fenv
