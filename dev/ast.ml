(** AST **)
open Printf

(* primitive operators *)
type prim1 = Add1 | Sub1 | Not | Print
type prim2 = Add | And | Lte | Lt 


type tag = int

(* Algebraic datatype for expressions *)
type 'a expr = 
  | Num of int64 * 'a
  | Bool of bool * 'a
  | Prim1 of prim1 * 'a expr * 'a
  | Prim2 of prim2 * 'a expr * 'a expr * 'a
  | Id of string * 'a
  | Let of string * 'a expr * 'a expr * 'a
  | If of 'a expr * 'a expr * 'a expr * 'a
  | Apply of string * 'a expr list * 'a

(* C function argument types *)
type ctype =
  | CAny
  | CInt
  | CBool

(* Function definitions *)
type fundef =
  | DefFun of string * string list * tag expr
  | DefSys of string * ctype list * ctype

let fundef_name(f : fundef) : string =
  match f with
  | DefFun (n, _, _) -> n
  | DefSys (n, _, _) -> n

(* Program including definitions and a body *)
type prog = fundef list * (tag expr)

(** Tagger that tags each node by its position in the AST*)
let tag (e : 'a expr) : tag expr =
  let rec help (e : 'a expr) (cur : tag) : (tag expr * tag) =
    match e with
    | Prim1(op, e1, _) ->
      let (tag_e, next_tag) = help e1 (cur + 1) in
      (Prim1(op, tag_e, cur), next_tag)
    | Prim2 (op,e1,e2,_) ->
      let (tag_e1,next_tag_1) = help e1 (cur +1) in
      let (tag_e2,next_tag_2) = help e2 next_tag_1 in 
      (Prim2 (op,tag_e1,tag_e2,cur),next_tag_2)
    | Num (n,_) -> (Num (n,cur),cur+1)
    | Id (x,_) -> (Id (x,cur),cur+1)
    | Bool (b,_) -> (Bool (b,cur),cur+1)
    | Let (id,bind,body,_) ->
      let (tag_bind,next_tag_1) = help bind (cur+1) in
      let (tag_body,next_tag_2) = help body (next_tag_1) in
      (Let (id,tag_bind,tag_body,cur),next_tag_2)
    | (If(cond,tbranch,fbranch,_)) ->
      let (tag_cond,next_tag_1) = help cond (cur+1) in 
      let (tag_tbranch,next_tag_2) = help tbranch (next_tag_1) in
      let (tag_fbranch,next_tag_3) = help fbranch (next_tag_2) in
      (If (tag_cond,tag_tbranch,tag_fbranch,cur),next_tag_3)
    | Apply (name, e, _) -> (
      let (tag_list_expr,next_tag)= (tag_list e (cur+1)) in
      
      (Apply (name, tag_list_expr ,cur), next_tag))
      (*
      let (tag_e, next_tag) = 
      help (hd e) (cur + 1) in 
      (Apply (name,tag_e,cur),next_tag)
        *)
      
      (*  *)
      and tag_list (expr_list:'a expr list) (cur : tag) :  (tag expr list * tag) =
      match expr_list with
      | h::t -> 
        (let (tag_h,next_tag) = help h (cur) in
        let (res,res_tag) =(tag_list t next_tag) in
        ([tag_h] @ res,res_tag))
      | [] -> ([],cur)
      

      in
      
  let (tagged, _) = help e 1 in tagged;;(*
let ayuda (list : 'a expr) : tag expr =
  let curr = 0 
  List.map (fun l -> let (a,b) = (help l curr)) list *)
(* Pretty printing - used by testing framework *)
let rec string_of_expr(e : tag expr) : string = 

  match e with
  | Num (n,_) -> Int64.to_string n
  | Bool (b,_) -> if b then "true" else "false"
  | Id (s,_) -> s
  | Prim1 (op, e,_) -> sprintf "(%s %s)" 
    (match op with
    | Add1 -> "add1"
    | Sub1 -> "sub1"
    | Not -> "not"
    | Print -> "print") (string_of_expr e) (* remove the print case when providing the sys interface *)
  | Prim2 (op, e1, e2,_) -> sprintf "(%s %s %s)" 
    (match op with 
    | Add -> "+"
    | And -> "and"
    | Lte -> "<="
    | Lt -> "<") (string_of_expr e1) (string_of_expr e2)
  | Let (x, e1, e2,_) -> sprintf "(let (%s %s) %s)" x (string_of_expr e1) (string_of_expr e2) 
  | If (e1, e2, e3,_) -> sprintf "(if %s %s %s)" (string_of_expr e1) (string_of_expr e2) (string_of_expr e3)
    

  | Apply (fe, ael,_) -> sprintf "(%s %s)" fe (String.concat " " (List.map string_of_expr ael))


(** functions below are not used, would be used if testing the parser on defs **)

(* Pretty printing C types - used by testing framework *)
let string_of_ctype(t : ctype) : string =
match t with
| CAny -> "any"
| CInt -> "int"
| CBool -> "bool"

(* Pretty printing function definitions - used by testing framework *)
let string_of_fundef(d : fundef) : string =
  match d with
  | DefFun (name, arg_ids, body) -> sprintf "(def (%s %s) %s)" name (String.concat " " arg_ids) (string_of_expr body)
  | DefSys (name, arg_types, ret_type) -> sprintf "(defsys %s %s -> %s)" name (String.concat " " (List.map string_of_ctype arg_types)) (string_of_ctype ret_type)

(* Pretty printing a program - used by testing framework *)
let string_of_prog(p : prog) : string =
  let fundefs, body = p in
  String.concat "\n" ((List.map string_of_fundef fundefs) @ [string_of_expr body])
