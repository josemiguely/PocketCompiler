(** AST **)
open Printf

(* primitive operators *)
type prim1 = Add1 | Sub1 | Not | Print
type prim2 = Add | And | Lte | Lt | Mult | Div | Sub | Get | Or


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
  | Tuple of 'a expr list * 'a
  | Set of 'a expr  * 'a expr  * 'a expr * 'a
  | Lambda of string list * 'a expr * 'a
  | LamApply of 'a expr * 'a expr list * 'a (* Lamba expr is applied to list of arguments*)
  | LetRec of (string * string list * 'a expr) list * 'a expr * 'a
  

(* C function argument types *)
type ctype =
  | CAny
  | CInt
  | CBool
  | CTuple of ctype list

(* Function definitions *)
type fundef =
  | DefFun of string * string list * tag expr (*Name of function, Name of arguments, Expression*)
  | DefSys of string * ctype list * ctype
  
let fundef_name (f : fundef) : string =
  match f with
  | DefFun (n, _, _) -> n
  | DefSys (n, _, _) -> n
  

(* Program including definitions and a body *)
type prog = fundef list * (tag expr)
(* type prog = decl list * (tag expr) *)

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
    | Set (variable,pos,replace,_) -> (
      let (variable,next_tag) = help variable (cur +1) in 
      let (tag_pos,next_tag2) = help pos (next_tag) in 
      let (tag_replace,next_tag3) = help replace (next_tag2) in
      (Set (variable,tag_pos,tag_replace,cur),next_tag3))
    | Tuple (list_expr,_) -> (
      let (tag_list_exp, next_tag) = (tag_list list_expr (cur+1)) in 
      (Tuple (tag_list_exp,cur),next_tag) 
    )
    | Apply (name, e, _) -> (
      let (tag_list_expr,next_tag)= (tag_list e (cur+1)) in
      (Apply (name, tag_list_expr ,cur), next_tag))
    | Lambda(id_list,body,_) -> 
      let (tag_body,next_tag_1) = help body (cur+1) in
      (Lambda(id_list,tag_body,cur),next_tag_1)
    | LamApply(lambda,arg_list,_) -> 
      let (tag_lambda,next_tag_1) = help lambda (cur+1) in
      let (tag_list_arg, next_tag_2) = (tag_list arg_list (next_tag_1)) in
      (LamApply (tag_lambda,tag_list_arg,cur),next_tag_2)     
    | LetRec(fun_list,expr,_) -> 
      let (tag_fun_list,next_tag_1) = fun_tags fun_list (cur+1) in 
      let (tag_expr,next_tag_2) = help expr next_tag_1 in 
      (LetRec (tag_fun_list,tag_expr,cur), next_tag_2)

    
      and fun_tags (fun_list : (string * string list * 'a expr) list) (cur : tag): ((string * string list * tag expr) list * tag) =
      match fun_list with 
        | h::t -> 
          (match h with
          | (id , str_list,expr) ->  let (tag_expr,next_tag) = help expr (cur) in
                                     let (tag_tail, res_tag) = fun_tags t next_tag in 
                                    ([(id,str_list,tag_expr)] @ tag_tail, res_tag))
        | [] -> ([],cur)





      and tag_list (expr_list:'a expr list) (cur : tag) :  (tag expr list * tag) =
      match expr_list with
      | h::t -> 
        (let (tag_h,next_tag) = help h (cur) in
        let (res,res_tag) =(tag_list t next_tag) in
        ([tag_h] @ res,res_tag))
      | [] -> ([],cur)
      

      in
      
  let (tagged, _) = help e 1 in tagged;;

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
    | Or -> "or"
    | Lte -> "<="
    | Lt -> "<"
    | Sub -> "-"
    | Div -> "/"
    | Mult -> "*"
    | Get -> "get") (string_of_expr e1) (string_of_expr e2)
  | Let (x, e1, e2,_) -> sprintf "(let (%s %s) %s)" x (string_of_expr e1) (string_of_expr e2) 
  | If (e1, e2, e3,_) -> sprintf "(if %s %s %s)" (string_of_expr e1) (string_of_expr e2) (string_of_expr e3)
  | Apply (fe, ael,_) -> sprintf "(%s %s)" fe (String.concat " " (List.map string_of_expr ael))
  | Tuple (exprs,_) -> sprintf "(tup %s)" (string_of_exprs exprs) 
  | Set (e, k, v,_) -> sprintf "(set %s %s %s)" (string_of_expr e) (string_of_expr k) (string_of_expr v)
  | Lambda (params, body,_) -> sprintf "(lambda (%s) %s)" (String.concat " " params) (string_of_expr body)
  | LamApply (fe, ael,_) -> sprintf "(%s %s)" (string_of_expr fe) (String.concat " " (List.map string_of_expr ael))
  | LetRec (recs, body,_) -> sprintf "(letrec (%s) %s)" (String.concat " " (List.map (
      fun (name, params, body) -> 
        sprintf "(%s %s)" name (string_of_expr (Lambda (params, body,-1)))
        ) recs
      )) (string_of_expr body)

  and string_of_exprs (e: 'a expr list) : string = 
      match e with
      | [] -> ""
      | h :: t -> " " ^ (string_of_expr h) ^ (string_of_exprs t) 


(** functions below are not used, would be used if testing the parser on defs **)

(* Pretty printing C types - used by testing framework *)
let rec string_of_ctype(t : ctype) : string =
match t with
| CAny -> "any"
| CInt -> "int"
| CBool -> "bool"
| CTuple types -> 
        let rec string_of_types =
            fun ls -> (match ls with
            | [] -> ""
            | e::l -> e ^ "," ^ string_of_types l) in
        "("^string_of_types (List.map string_of_ctype types)^")"


(* Pretty printing function definitions - used by testing framework *)
let string_of_fundef(d : fundef) : string =
  match d with
  | DefFun (name, arg_ids, body) -> sprintf "(def (%s %s) %s)" name (String.concat " " arg_ids) (string_of_expr body)
  | DefSys (name, arg_types, ret_type) -> sprintf "(defsys %s %s -> %s)" name (String.concat " " (List.map string_of_ctype arg_types)) (string_of_ctype ret_type)


let string_of_decl (d : fundef) : string =
  match d with
  | DefFun (name, arg_ids, body) -> sprintf "(def (%s %s) %s)" name (String.concat " " arg_ids) (string_of_expr body)
  | DefSys (name, arg_types, ret_type) -> sprintf "(defsys %s %s -> %s)" name (String.concat " " (List.map string_of_ctype arg_types)) (string_of_ctype ret_type)

(* Pretty printing a program - used by testing framework *)
let string_of_prog(p : prog) : string =
  let decl, body = p in
  String.concat "\n" ((List.map string_of_decl decl) @ [string_of_expr body])
