(** AST **)
open Printf

(* primitive operators *)
type prim1 = Add1 | Sub1
type prim2 = Add | And | Lte 

(* Algebraic datatype for expressions *)
type 'a expr = 
  | Num of int64 * 'a
  | Bool of bool * 'a
  | Prim1 of prim1 * 'a expr * 'a
  | Prim2 of prim2 * 'a expr * 'a expr * 'a
  | Id of string * 'a
  | Let of string * 'a expr * 'a expr * 'a
  | If of 'a expr * 'a expr * 'a expr * 'a
  

type tag = int


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
    | Sub1 -> "sub1") (string_of_expr e)
  | Prim2 (op, e1, e2,_) -> sprintf "(%s %s %s)" 
    (match op with 
    | Add -> "+"
    | And -> "and"
    | Lte -> "<=") (string_of_expr e1) (string_of_expr e2)
  | Let (x, e1, e2,_) -> sprintf "(let (%s %s) %s)" x (string_of_expr e1) (string_of_expr e2) 
  | If (e1, e2, e3,_) -> sprintf "(if %s %s %s)" (string_of_expr e1) (string_of_expr e2) (string_of_expr e3)
