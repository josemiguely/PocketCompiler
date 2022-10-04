open Ast
open Asm
open Printf






type kind = 
  | ArgKind
  | LocalKind

type env = (string * int * kind) list


let rec lookup name env =
  match env with
  | [] -> failwith (sprintf "Identifier %s is not found in environment" name)
  | (n,i,kind)::rest->
    if n = name then (i,kind) else (lookup name rest)


(*Adds variable with identifier "name" to "env" with a specific "kind"*)
let add name env kind : (env * int) =
  (* print_string name; *)
  let slot = 1 + (List.length env) in
  ((name,slot,kind)::env,slot)



  type funenv = (string * int) list

  let rec lookup_fun name fun_env arity : int =
    match fun_env with
    | [] -> failwith (sprintf "Undefined function: %s" name)
    | (n,i)::rest->
      if (n = name) 
        then 
          (if arity = i 
            then (i)
            else (failwith (sprintf "Arity mismatch: %s expected %i arguments but got %i" name i arity)))
      else (lookup_fun name rest arity)

  
let add_fun name arity fun_env : (funenv) =
  ((name,arity)::fun_env)


    let save_register_arguments_before_call = [IPush(Reg(R9));IPush(Reg(R8));IPush(Reg(RCX));IPush(Reg(RDX));IPush(Reg(RSI));IPush(Reg(RDI))]

   


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

   (*Save arguments of actual function / caller before calling a new function*)
   let rec save_arguments_before_call (arg_count : int) (track_count : int) : instruction list  =
   (* print_int track_count;  *)
   if track_count == arg_count
      then []
    else 
      if track_count<6 then  save_arguments_before_call arg_count (track_count+1) @ [IPush(List.nth register_arguments (track_count))] 
      else [IMov(Reg(RAX),RegOffset(RBP,"+",8))] @ [IPush (Reg(RAX))]



  (*Restore arguments after calling a new function*)
  let rec restore_arguments_after_call (arg_count : int) (track_count : int) : instruction list  =
  (* print_int track_count;   *)
  if (track_count == arg_count || track_count>=6)
     then []
   else 
    (*if track_count<6 then *)[IPop(List.nth register_arguments (track_count))] @ restore_arguments_after_call arg_count (track_count+1)
    (* else [IPop (RegOffset(RBP,"+",8*(track_count-7+2)))] *)
    


  (** Compile AST expressions *)
  let rec compile_expr (e : tag expr) (env : env) (funenv : funenv) (arg_count : int)  : instruction list =
    
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
      | Not -> (compile_expr expr env funenv arg_count) @ test_boolean_instruction  @[IMov (Reg(R10),Const(not_mask))] @ [IXor (Reg(RAX),Reg(R10))] 
      | Add1 -> (compile_expr expr env funenv arg_count) @ test_number_instruction @ [IAdd (Reg(RAX),Const(2L))] 
      | Sub1 -> (compile_expr expr env funenv arg_count) @ test_number_instruction @[IAdd (Reg(RAX),Const(-2L))] 
      | Print -> (compile_expr expr env funenv arg_count) @ [IMov(Reg(RDI),Reg(RAX))] @ [ICall("print")])
    | Let (x,e,b,_) -> 
      let (env',slot) = add x env LocalKind in
      (*Compile the binding, and get the result into RAX*)
      (compile_expr e env funenv arg_count) 
      (* Copy the result in RAX into the appropriate stack slot*)
      @ [IMov (RegOffset(RBP,"-",8*slot),Reg(RAX))]
      (* Compile the body, given that x is in the correct slot when it's needed*)
      @ (compile_expr b env' funenv arg_count)
    | Id (x,_) -> let (slot,kind) = (lookup x env) in
    (match kind with
      | LocalKind -> [IMov (Reg(RAX),RegOffset(RBP,"-",8*slot))]
      | ArgKind  ->(
          if slot<=6 then [IMov (Reg(RAX),List.nth register_arguments (slot-1))] (*RegOffset(RBP,"+",1*slot)*)
          else [IMov (Reg(RAX),RegOffset(RBP,"+",8*(slot-7+2)))]))(* *)
    | If (cond,thn,els,tag) ->
      let else_label = sprintf "false_branch_%d" tag in
      let done_label = sprintf "done_%d" tag in
      (compile_expr cond env funenv arg_count) @
      test_boolean_instruction @
      [
       ICmp(Reg(RAX),Const(const_true));
       IJne(else_label)
      ]
      @ (compile_expr thn env funenv arg_count)
      @ [ IJmp(done_label); ILabel(else_label)]
      @ (compile_expr els env funenv arg_count)
      @ [ ILabel (done_label)] 
    | Prim2 (prim2,expr1,expr2,tag) -> (
      let (env',slot1) = add "izq" env LocalKind in
      let (env'',slot2) = add "der" env' LocalKind in
      let scaffold = (prim2_scaffold expr1 expr2 slot1 slot2 env'' prim2 tag funenv arg_count) in
      match prim2 with
      | Add ->  scaffold @ [IAdd (Reg(RAX),RegOffset(RBP,"-",8*slot2))] 
      | Sub -> scaffold @ [ISub (Reg(RAX),RegOffset(RBP,"-",8*slot2))]
      | Mult -> scaffold @ [IMult(Reg(RAX),RegOffset(RBP,"-",8*slot2))]
      | Div -> scaffold @ [IDiv (Reg(RAX),RegOffset(RBP,"-",8*slot2))]
      | And -> scaffold
      | Lt -> 
        let less_label = sprintf "less_%d" tag in
        scaffold @ [ICmp (Reg(RAX),RegOffset(RBP,"-",8*slot2))] @ [IMov (Reg(RAX),Const(const_true))] @ [IJl (less_label)] @ [IMov (Reg(RAX),Const(const_false))] @ [ILabel (less_label)]
      | _ -> failwith("Unexpected binary operation") ) 
  | Apply(id,expr_list,_) -> 
    let instr = arg_list_evaluator expr_list env 0 funenv arg_count in (*First we eval Apply arguments*)
    let arg_number = List.length expr_list in
    let _ = lookup_fun id funenv arg_number in
    let arg_more_than_6_offset = (arg_number-6)*8 in
    let res = Int64.of_int (max arg_more_than_6_offset 0) in
    save_arguments_before_call arg_count 0 (* save register arguments*)
    @ instr (* Push arguments from 7 to arg_number*)
    @ [ICall(id)] (* getICall(id) *) 
    @ [IAdd(Reg(RSP),Const(res))] 
    @ restore_arguments_after_call arg_count 0
    
    (* @ pop_arguments_after_call restore the current values of the caller-save argument registers in reverse order from saving them *)


      
  (** Creates common scaffold for binary operations. For And operation it also includes short-circuit evaluation**)
    and prim2_scaffold (e1: tag expr) (e2 : tag expr) (slot1 : int)(slot2 : int)(env :env)(prim2 : prim2 )(tag:tag) (funenv : funenv) (arg_count : int) : instruction list =
    
    match prim2 with
    | And -> 
      let done_label = sprintf "done_%d" tag in
      (compile_expr e1 env funenv arg_count) @ 
      test_boolean_instruction @
      [
        IMov(Reg(R10),Const(const_false));
        ICmp(Reg(RAX),Reg(R10));
        IJe(done_label);
       ]
       @ [IMov (RegOffset(RBP,"-",8*slot1),Reg(RAX))]
       @ (compile_expr e2 env funenv arg_count)
       @ test_boolean_instruction
       @[IMov (RegOffset(RBP,"-",8*slot2),Reg(RAX))]
       @[IMov (Reg(RAX),RegOffset(RBP,"-",8*slot1))]
       @[IAnd (Reg(RAX),RegOffset(RBP,"-",8*slot2))]
       @[IJmp (done_label)]
       @[ILabel(done_label)]
       
    | _ ->
    (compile_expr e1 env funenv arg_count)
    @ test_number_instruction
    @ [IMov (RegOffset(RBP,"-",8*slot1),Reg(RAX))]
    @ (compile_expr e2 env funenv arg_count)
    @ test_number_instruction
    @ [IMov (RegOffset(RBP,"-",8*slot2),Reg(RAX))]
    @ [IMov (Reg(RAX),RegOffset(RBP,"-",8*slot1))]
  


    and arg_list_evaluator (arg_exp_list : tag expr list) (env : env) (count:int) (funenv : funenv) (arg_count : int)  =
    (* print_int count;  *)
    match arg_exp_list with
    | h::t ->
      if count<6 
        then (arg_list_evaluator t env (count+1) funenv arg_count) @ compile_expr h env funenv arg_count @ [IMov(List.nth register_arguments count,Reg(RAX))]
        else (arg_list_evaluator t env (count+1) funenv arg_count) @ compile_expr h env funenv arg_count @ [IPush(Reg(RAX))]
    | [] -> []

let rec add_list list_variables env kind =
   match list_variables with
   | h::t -> let (new_env,_) = add h env kind in 
                add_list t new_env kind
   | [] -> env


let rec var_count(ex: tag expr) : int  =
  match ex with 
  | Prim1(_,e,_) -> 1 + var_count e
  | Prim2(_,e1,e2,_) -> 1 + max (var_count e1) (var_count e2)
  | Let(_ ,e,body,_) -> 1 + max (var_count e) (var_count body)
  | If(cond,tbranch,fbranch,_)-> 1 + max (var_count cond) (max (var_count tbranch) (var_count fbranch)) (* Arreglar pq cond (max del max)también puede*)
  | Apply (_, e, _) -> 1 + (List.fold_left max 0 (List.map var_count e)) (*agregar max de la lista antes del fold*)
  |_ -> 1

  
let compile_decl (fdef:fundef) (fun_env : funenv) : (string * funenv) =
  match fdef with
  | DefFun (id,arg_list,expr) -> 
    let arg_count =  (List.length arg_list) in
    let new_env = add_list arg_list [] ArgKind in
    let new_fun_env = add_fun id arg_count fun_env in
    let count_of_var = Int64.of_int (16* 16 * (var_count expr)) in
    let decl = asm_to_string([ILabel(id);IPush(Reg(RBP));IMov(Reg(RBP),Reg(RSP));ISub(Reg(RSP),Const(count_of_var))] @ (compile_expr (tag expr) new_env new_fun_env arg_count) @ [IMov(Reg(RSP),Reg(RBP))] @ [IPop (Reg(RBP))]  @[IRet]) in
    (decl,new_fun_env)
  | _ -> failwith("error")
  


let rec compile_list_fundef (f_list:fundef list) (fun_env : funenv) : (string * funenv) =
  match f_list with
  | h::t -> 
    let (head,new_fenv) = compile_decl h fun_env in
    let (rest,final_fenv) = compile_list_fundef t new_fenv in
    ("\n"^ head ^ rest,final_fenv)
  | [] -> ("",fun_env) 

let compile_prog p  : string =
  let flist, e = p in
  let (functions,funenv) = compile_list_fundef flist [] in
  let instrs = compile_expr (tag e) [] funenv 0 in
  let count_of_var = 16* 16 * (var_count e) in
  let prelude =sprintf "section .text
global our_code_starts_here
extern error
extern print
our_code_starts_here:
push RBP
mov RBP, RSP
sub RSP, 0x%x\n" count_of_var in

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




