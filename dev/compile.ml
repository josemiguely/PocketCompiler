open Ast
open Asm
open Printf


type kind = string

type env = (string * int * kind) list


let rec lookup name env =
  match env with
  | [] -> failwith (sprintf "Identifier %s is not found in environment" name)
  | (n,i,kind)::rest->
    if n = name then (i,kind) else (lookup name rest)


(*Adds variable with identifier "name" to "env" with a specific "kind"*)
let add name env kind : (env * int) =
  let slot = 1 + (List.length env) in
  ((name,slot,kind)::env,slot)


    let save_arguments_before_call = [IPush(Reg(R9));IPush(Reg(R8));IPush(Reg(RCX));IPush(Reg(RDX));IPush(Reg(RSI));IPush(Reg(RDI))]

    let pop_arguments_after_call = [IPop(Reg(RDI));IPop(Reg(RSI));IPop(Reg(RDX));IPop(Reg(RCX));IPop(Reg(R8));IPop(Reg(R9))]
    let const_true = 0xFFFFFFFFFFFFFFFFL (*true value = only 1's*) 
    let const_false = 0x7FFFFFFFFFFFFFFFL (*false value = Most significant bit with 0 and then all 1's*)

    let not_mask = 0x8000000000000000L (*Not Mask that only captures most significant bit *)

    let min_int = Int64.div Int64.min_int 2L
    let max_int = Int64.div Int64.max_int 2L
    
    let test_number = 0x0000000000000001L

   let test_number_instruction = [ITest (Reg(RAX),Const(test_number))] @ [IJnz("error_not_number")]

   let test_boolean_instruction = [ITest (Reg(RAX),Const(test_number))] @ [IJz("error_not_boolean")]
  
   let register_arguments = [Reg(RDI);Reg(RSI);Reg(RDX);Reg(RCX);Reg(R8);Reg(R9)]

  (** Compile AST expressions *)
  let rec compile_expr (e : tag expr) (env : env) : instruction list =
    
    match e with
    | Num (n,_) ->
      let shifted = (Int64.shift_left n 1) in
      if n > max_int|| n < min_int then
        failwith ("Integer overflow: " ^ (Int64.to_string n))
      else
        [IMov (Reg(RAX),Const(shifted))]
    | Bool (true,_) ->  [IMov (Reg(RAX),Const(const_true))]
    | Bool (false,_) ->  [IMov (Reg(RAX),Const(const_false))]
    | Prim1 (prim1,expr,_) -> (
      match prim1 with 
      | Not -> (compile_expr expr env) @ test_boolean_instruction  @[IMov (Reg(R10),Const(not_mask))] @ [IXor (Reg(RAX),Reg(R10))] 
      | Add1 -> (compile_expr expr env) @ test_number_instruction @ [IAdd (Reg(RAX),Const(2L))] 
      | Sub1 -> (compile_expr expr env) @ test_number_instruction @[IAdd (Reg(RAX),Const(-2L))] 
      | Print -> (compile_expr expr env) @ [IMov(Reg(RDI),Reg(RAX))] @ [ICall("print")])
    | Let (x,e,b,_) -> 
      let (env',slot) = add x env "local" in
      (*Compile the binding, and get the result into RAX*)
      (compile_expr e env) 
      (* Copy the result in RAX into the appropriate stack slot*)
      @ [IMov (RegOffset(RBP,"-",8*slot),Reg(RAX))]
      (* Compile the body, given that x is in the correct slot when it's needed*)
      @ (compile_expr b env')
    | Id (x,_) -> let (slot,kind) = (lookup x env) in
      if kind=="local"
        then [IMov (Reg(RAX),RegOffset(RBP,"-",8*slot))]
         else if slot<=6 then [IMov (Reg(RAX),List.nth register_arguments (slot-1))] (*RegOffset(RBP,"+",1*slot)*)
         else [IMov (Reg(RAX),RegOffset(RBP,"+",8*(slot+2)))]
    | If (cond,thn,els,tag) ->
      let else_label = sprintf "false_branch_%d" tag in
      let done_label = sprintf "done_%d" tag in
      (compile_expr cond env) @
      test_boolean_instruction @
      [
       ICmp(Reg(RAX),Const(const_true));
       IJne(else_label)
      ]
      @ (compile_expr thn env)
      @ [ IJmp(done_label); ILabel(else_label)]
      @ (compile_expr els env)
      @ [ ILabel (done_label)] 
    | Prim2 (prim2,expr1,expr2,tag) -> (
      let (env',slot1) = add "izq" env "local" in
      let (env'',slot2) = add "der" env' "local" in
      let scaffold = (prim2_scaffold expr1 expr2 slot1 slot2 env'' prim2 tag) in
      match prim2 with
      | Add ->  scaffold @ [IAdd (Reg(RAX),RegOffset(RBP,"-",8*slot2))] 
      | And -> scaffold
      | Lt -> 
        let less_label = sprintf "less_%d" tag in
        scaffold @ [ICmp (Reg(RAX),RegOffset(RBP,"-",8*slot2))] @ [IMov (Reg(RAX),Const(const_true))] @ [IJl (less_label)] @ [IMov (Reg(RAX),Const(const_false))] @ [ILabel (less_label)]
      | _ -> failwith("Unexpected binary operation") ) 
  | Apply(id,expr_list,_) -> 
    let instr = arg_list_evaluator expr_list env 0 in
    let arg_number = List.length expr_list in
    let arg_more_than_6_offset = (arg_number-6)*8 in
    let res = Int64.of_int (max arg_more_than_6_offset 0) in
    save_arguments_before_call 
    @ instr (* Push arguments from 7 to arg_number*)
    @ [ICall(id)] 
    @ [IAdd(Reg(RSP),Const(res))] 
    @ pop_arguments_after_call (*restore the current values of the caller-save argument registers in reverse order from saving them*)


      
  (** Creates common scaffold for binary operations. For And operation it also includes short-circuit evaluation**)
    and prim2_scaffold (e1: tag expr) (e2 : tag expr) (slot1 : int)(slot2 : int)(env :env)(prim2 : prim2 )(tag:tag) : instruction list =
    
    match prim2 with
    | And -> 
      let done_label = sprintf "done_%d" tag in
      (compile_expr e1 env) @ 
      test_boolean_instruction @
      [
        IMov(Reg(R10),Const(const_false));
        ICmp(Reg(RAX),Reg(R10));
        IJe(done_label);
       ]
       @ [IMov (RegOffset(RBP,"-",8*slot1),Reg(RAX))]
       @ (compile_expr e2 env)
       @ test_boolean_instruction
       @[IMov (RegOffset(RBP,"-",8*slot2),Reg(RAX))]
       @[IMov (Reg(RAX),RegOffset(RBP,"-",8*slot1))]
       @[IAnd (Reg(RAX),RegOffset(RBP,"-",8*slot2))]
       @[IJmp (done_label)]
       @[ILabel(done_label)]
       
    | _ ->
    (compile_expr e1 env)
    @ test_number_instruction
    @ [IMov (RegOffset(RBP,"-",8*slot1),Reg(RAX))]
    @ (compile_expr e2 env)
    @ test_number_instruction
    @ [IMov (RegOffset(RBP,"-",8*slot2),Reg(RAX))]
    @ [IMov (Reg(RAX),RegOffset(RBP,"-",8*slot1))]
  


    and arg_list_evaluator (arg_exp_list : tag expr list) (env : env) (count:int)  =
    match arg_exp_list with
    | h::t ->
      if count<6 
        then (arg_list_evaluator t env (count+1)) @ compile_expr h env @ [IMov(List.nth register_arguments count,Reg(RAX))]
        else (arg_list_evaluator t env (count+1)) @ compile_expr h env @ [IPush(Reg(RAX))]
    | [] -> []

let rec add_list list_variables env kind =
   match list_variables with
   | h::t -> let (new_env,_) = add h env kind in 
                add_list t new_env kind
   | [] -> env

let compile_decl (fdef:fundef) : string =
  match fdef with
  | DefFun (id,arg_list,expr) -> 
    (* let arg_count =   Int64.of_int (List.length arg_list * 8) in *)
    let new_env = add_list arg_list [] "argument" in
    asm_to_string([ILabel(id);IPush(Reg(RBP));IMov(Reg(RBP),Reg(RSP));ISub(Reg(RSP),Const(112L))] @ (compile_expr (tag expr) new_env) @ [IMov(Reg(RSP),Reg(RBP))] @ [IPop (Reg(RBP))]  @[IRet])
  | _ -> failwith("error")
  

let rec compile_list_fundef (f_list:fundef list) : string =
  match f_list with
  | h::t -> "\n"^compile_decl h ^ compile_list_fundef t
  | [] -> "" 

let compile_prog p  : string =
  let flist, e = p in
  let functions = compile_list_fundef flist in
  let instrs = compile_expr (tag e) [] in
  let prelude ="section .text
global our_code_starts_here
extern error
extern print
our_code_starts_here:
push RBP
mov RBP, RSP
sub RSP, 0x70\n" in
let prologue ="mov RSP, RBP
pop RBP
" in

let error_functions ="
error_not_number:
mov RSI,RAX
mov RDI,0x1
call error

error_not_boolean:
mov RSI,RAX
mov RDI,0x2
call error
" in
prelude ^  asm_to_string (instrs) ^ prologue ^ asm_to_string([IRet]) ^  functions ^ error_functions




