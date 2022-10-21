open Ast
open Asm
open Printf

(* Constants*)
let const_true = 0xFFFFFFFFFFFFFFFFL (*true value = only 1's*) 
let const_false = 0x7FFFFFFFFFFFFFFFL (*false value = Most significant bit with 0 and then all 1's*)

let not_mask = 0x8000000000000000L (*Not Mask that only captures most significant bit *)

let min_int = Int64.div Int64.min_int 2L
let max_int = Int64.div Int64.max_int 2L

let test_number = 0x0000000000000001L (*Number used in some tests instruction*)

(* let test_tuple = 0x00000000000000001L *)

let save_register_arguments_before_call = 
[IPush(Reg(R9));IPush(Reg(R8));IPush(Reg(RCX));IPush(Reg(RDX));IPush(Reg(RSI));IPush(Reg(RDI))]

let pop_arguments_after_call = 
[IPop(Reg(RDI));IPop(Reg(RSI));IPop(Reg(RDX));IPop(Reg(RCX));IPop(Reg(R8));IPop(Reg(R9))]


let test_number_instruction = [ITest (Reg(RAX),Const(test_number))] @ [IJnz("error_not_number")]

let test_boolean_instruction = [ITest (Reg(RAX),Const(test_number))] @ [IJz("error_not_boolean")]

(* let test_tuple_instruction = (getITest ) *)

let register_arguments = [Reg(RDI);Reg(RSI);Reg(RDX);Reg(RCX);Reg(R8);Reg(R9)]


type kind = 
  | ArgKind
  | LocalKind

type env = (string * int * kind) list


(*Pure functions for registers*)

let getRAX = RAX

let getRSP = RSP

let getR10 = R10

let getRBP = RBP

let getRDI = RDI

let getRSI = RSI

let getRDX = RDX

let getRCX = RCX

let getR15 = R15

let getR11 = R11


(*Pure functions for arguments*)

let getConst ( num:int64 ) : arg =
(***Recieves num of type int64 and returns Const(num) of type arg*)
  Const(num)

let getReg ( reg1:reg ) : arg =
(**Recieves reg1 of type reg. Returns [Reg(reg1)] of type arg*)
  Reg(reg1)


let getRegOffset (reg:reg) (op : string) (offset: int) : arg =
(** Recieves a register that with a operation will offset an amount. Returns [[RegOffset(reg,op,offset)]]*)
  RegOffset(reg,op,offset)

  let getRegOffsetRR (reg:reg) (op : string) (reg2: reg) : arg =
    (** Recieves a register that with a operation will offset a reg amount. Returns [[RegOffset(reg,op,reg)]]*)
    RegOffsetRR(reg,op,reg2)

(*Pure functions for instructions*)


let getIMov (arg1 : arg) (arg2: arg) : instruction list =
(**Moves second arg to first arg. Returns [[IMov (arg1,arg2)]]*)
  [IMov(arg1,arg2)]

let getIAdd (arg1 : arg) (arg2 : arg) =
(**Add first arg with second arg. Returns [[IAdd (arg1,arg2)]]*)
  [IAdd (arg1,arg2)]

let getISub (arg1 : arg) (arg2 : arg) =
  [ISub (arg1,arg2)]
  

let getIMult (arg1 : arg) (arg2 : arg) =
  [IMult(arg1,arg2)]

let getIDiv (arg1 : arg) =
  [IDiv(arg1)]

let getIAnd (arg1 : arg) (arg2 : arg) =
  [IAnd(arg1,arg2)]

let getICmp (arg1 : arg) (arg2 : arg) =
  [ICmp(arg1,arg2)]


let getIJe (label : string ) =
  [IJe(label)]


let getIJl (label : string ) =
  [IJl(label)]


let getIJne (label : string ) =
  [IJe(label)]


let getIJmp (label : string ) =
  [IJmp(label)]


let getIJz (label : string ) =
  [IJz(label)]


let getIJnz (label : string ) =
  [IJnz(label)]

let getITest (arg1 : arg) (arg2 : arg) =
  [ITest(arg1,arg2)]

let getIXor (arg1 : arg) (arg2 : arg)=
  [IXor(arg1,arg2)]

let getILabel (label : string ) =
  [ILabel(label)]

let getICall (label : string ) =
  [ICall(label)]

let getIPush (arg1:arg)  =
  [IPush (arg1)]

let getIPop (arg1:arg)  =
  [IPop (arg1)]

let getIShl (arg1 : arg) (arg2 : arg)=
  [IShl(arg1,arg2)]

let getISar (arg1 : arg) (arg2 : arg)=
  [ISar(arg1,arg2)]

let getICqo = [ICqo]

let getIRet = [IRet]


(*Compile_expr pure functions*)

let eNum (n:int64)= 
    let shifted = (Int64.shift_left n 1) in
      if n > max_int|| n < min_int then
        failwith ("Integer overflow: " ^ (Int64.to_string n))
      else
        [IMov (Reg(RAX),Const(shifted))]


let eBool (truth_value : int64) =
  [IMov (Reg(RAX),Const(truth_value))]

let eNot () =
  test_boolean_instruction  
  @ [IMov (Reg(R10),Const(not_mask))] 
  @ [IXor (Reg(RAX),Reg(R10))] 


let eAdd1Sub1 (instr : string) (number : int64) =
  test_number_instruction 
  @ [IPush (Reg(RAX))] (* Push Rax to recover its value after function call*)
  @ [IPush(Reg(RSI))] @ [IPush(Reg(RDI))] 
  @ [IMov(Reg(RDI),Reg(RAX))]
  @ [IMov(Reg(RSI),Const(2L))]
  @ [ICall(sprintf "check_overflow_%s" instr)]
  @ [IPop (Reg(RDI));IPop (Reg(RSI));IPop(Reg(RAX))]
  @ [IAdd (Reg(RAX),Const(number))]

let eAdd (slot2 : int) =
[IAdd (Reg(RAX),RegOffset(RBP,"-",8*slot2))]

let eSub (slot2 : int) =
  [ISub (Reg(RAX),RegOffset(RBP,"-",8*slot2))] 

let eMult (slot2 : int) =
  [IMult(Reg(RAX),RegOffset(RBP,"-",8*slot2))
                  ;ISar(Reg(RAX),Const(1L))]


let eDiv (slot2 : int) =
[IMov (Reg(R10),(RegOffset(RBP,"-",8*slot2)))] 
               @ [ICqo;IDiv(Reg(R10));IShl(Reg(RAX),Const(1L))]


let eLt (slot2 : int) (less_label : string) =
  [ICmp (Reg(RAX),RegOffset(RBP,"-",8*slot2))] 
  @ [IMov (Reg(RAX),Const(const_true))] @ [IJl (less_label)] 
  @ [IMov (Reg(RAX),Const(const_false))] @ [ILabel (less_label)]



  


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

    
(*Save arguments of actual function / caller before calling a new function*)
let rec save_arguments_before_call (arg_count : int) (track_count : int) : instruction list  =
(* print_int track_count;  *)
if track_count == arg_count
  then []
else 
  if track_count<6 then  save_arguments_before_call arg_count (track_count+1) @ [IPush(List.nth register_arguments (track_count))] 
  else [IMov(Reg(RAX),RegOffset(RBP,"+",8))] @ [IPush (Reg(RAX))]

 
  (* match expr_list with
    | _::t -> let (env',slot1) = add (sprintf "tuple_%d" tag) env LocalKind in
              [(env',slot1)] @ (add_expr_to_stack t env'(tag+1))
    | [] -> [] *)



(*Restore arguments after calling a new function*)
let rec restore_arguments_after_call (arg_count : int) (track_count : int) : instruction list  =
if (track_count == arg_count || track_count>=6)
    then []
  else 
  [IPop(List.nth register_arguments (track_count))] @ restore_arguments_after_call arg_count (track_count+1)
  
let call_function_one_argument (funct: string) : instruction list = 
  [IPush(Reg(RDI))] @[IMov(Reg(RDI),Reg(RAX))] @ [ICall(funct)] @ [IPop (Reg(RDI))]

let call_function_two_argument (funct: string) : instruction list = 
  [IPush(Reg(RSI));IPush(Reg(RDI))] @[IMov(Reg(RDI),Reg(RAX))] @ [ICall(funct)] @ [IPop (Reg(RDI));IPop (Reg(RSI))] 
  
  (** Compile AST expressions *)
  let rec compile_expr (e : tag expr) (env : env) (funenv : funenv) (arg_count : int)  : instruction list =
    match e with
    | Num (n,_) -> eNum(n)
    | Bool (true,_) ->  eBool(const_true)
    | Bool (false,_) ->  eBool(const_false)
    | Prim1 (prim1,expr,_) -> (
      match prim1 with 
      | Not -> (compile_expr expr env funenv arg_count) 
                @ eNot()
      | Add1 -> (compile_expr expr env funenv arg_count) 
                @ (eAdd1Sub1 "add" 2L)

      | Sub1 -> (compile_expr expr env funenv arg_count)    
               @ (eAdd1Sub1 "sub" (-2L))

      | Print -> (compile_expr expr env funenv arg_count) 
                @ (call_function_one_argument "print"))
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
          if slot<=6 then [IMov (Reg(RAX),List.nth register_arguments (slot-1))]
          else [IMov (Reg(RAX),RegOffset(RBP,"+",8*(slot-7+2)))]))
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
        | Add ->  scaffold @ eAdd(slot2)
        | Sub -> scaffold @ eSub(slot2)
        | Mult -> scaffold 
                  @ eMult(slot2)
        | Div -> scaffold 
                @ eDiv(slot2) 
        | And -> scaffold
        | Lt -> (
          let less_label = sprintf "less_%d" tag in
          scaffold 
          @ (eLt slot2 less_label))
        | Get -> (compile_expr expr1 env funenv arg_count) (*Compilo el numero de acceso*)
                @(getIMov (getReg getR10) (getReg getRAX) ) (*Dejo el resultado en R10*)
                @ (getIPush (getReg getR10)) (*Pusheo R10*)
                @ (compile_expr expr2 env funenv arg_count) (*Compilo la tupla*) (*Debo checkear que resultado es tuplas*)
                @ (getISub (getReg getRAX) (getConst (1L)))
                @ (getIPop (getReg getR10)) (*Recupero el n*)
                @ (getIAdd (getReg getR10) (getConst 1L)) (*Le sumo 1*)
                @ (getIMult (getReg getR10) (getConst 8L)) (*Lo multiplico por 8*)
                @ (getIAdd (getReg getR10) (getReg getRAX)) (*Le sumo el resultado de la tupla para despues desreferenciarlo*)
                @ (getIMov (getReg getRAX) (getRegOffset getR10 "+" 0))
                
                 
        | _ -> failwith("Unexpected binary operation") ) 
  | Apply(id,expr_list,_) -> 
    let instr = arg_list_evaluator expr_list env 0 funenv arg_count in (*First we eval Apply arguments*)
    let arg_number = List.length expr_list in
    let _ = lookup_fun id funenv arg_number in
    let arg_more_than_6_offset = (arg_number-6)*8 in
    let res = Int64.of_int (max arg_more_than_6_offset 0) in
    save_arguments_before_call arg_count 0 (* save register arguments*)
    @ instr (* Push arguments from 7 to arg_number*)
    @ [ICall(id)]  
    @ [IAdd(Reg(RSP),Const(res))] 
    @ restore_arguments_after_call arg_count 0

  | Set (_,_,_,_) -> (failwith("tonta"))
  | Tuple(expr_list,_) -> 
    (*Nmero de expresiones en la tupla*)
    let expr_number = List.length expr_list in
    (* (printf "expr_number %i" expr_number); *)
    
    if expr_number==0 then
        getIMov (getReg (getR10)) (getConst (Int64.of_int expr_number))
      @ getIMov (getRegOffset (getR15) "+"  ((8*0))) (getReg getR10) 
      @ (compile_tuple [] 0 expr_number)
    
    else
      let env_slot =(generate_list_env_slot expr_list env) in
      let finalenv = (fst (List.nth env_slot  (expr_number-1))) in
      let compiled = 
        (List.map2 ( fun x y -> 
          let res = compile_expr x finalenv funenv arg_count in
          let slot = (snd y) in 
          res @ (getIMov (getRegOffset (getRBP) "-" (8*slot))(getReg(getRAX)))) expr_list env_slot ) in
      let compiled_folded = (List.fold_left (fun x y-> x @ y) [] compiled) in
      compiled_folded 
      @ getIMov (getReg (getR10)) (getConst (Int64.of_int expr_number))
      @ getIMov (getRegOffset (getR15) "+"  ((8*0))) (getReg getR10) (*size de la tupla*)
      @ (compile_tuple env_slot 0 expr_number) (*Añado el resto de la tupla a HeapPointer y devuelvo Pointer*)

    
      
                

    (* let expr_count = (List.length expr_list) in
    let accum = 1 in
    (eTuple expr_list expr_count tag accum env funenv arg_count) *)


    (* (getIMov (getReg(getR10)) (getConst (Int64.of_int expr_count))) 
    @ (getIMov (getRegOffset getR15 "+" 0) (getReg getR10)) 
    @ (eTuple expr_list expr_count tag accum env funenv arg_count) *)
    (*Colocamos el size de la tupla*)
    (* (getIMov (getReg getR10) (getConst expr_number)) IMov(Reg(R10),Reg(Const(expr_number))  )  *)
    (*Evaluar cada elemento de la lista*)
     (* @ match expr_list with
      | h::t -> (getIMov (getRegOffset getR15 "+" 1) (getReg (compile_expr h env funenv arg_count))) 
      | [] -> getIMov (getReg getRAX) (getReg getR15) @
              getIAdd (getReg getRAX) (getConst 1L) @ (*tag de la tupla*)
              getIAdd (getReg getR15) (getConst (Int64.of_int (8*(tuples+1)))) n elementos = expr_numner *)
   
    
      
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
       @ [IMov (RegOffset(RBP,"-",8*slot2),Reg(RAX))]
       @ [IMov (Reg(RAX),RegOffset(RBP,"-",8*slot1))]
       @ [IAnd (Reg(RAX),RegOffset(RBP,"-",8*slot2))]
       @ [IJmp (done_label)]
       @ [ILabel(done_label)]

    | Div ->  
      (compile_expr e1 env funenv arg_count)
      @ test_number_instruction
      @ [IMov (RegOffset(RBP,"-",8*slot1),Reg(RAX))]
      @ (compile_expr e2 env funenv arg_count)
      @ test_number_instruction
      @ [IMov (RegOffset(RBP,"-",8*slot2),Reg(RAX))]
      @ call_function_one_argument("check_non_zero_denominator")
      @ [IMov (Reg(RAX),RegOffset(RBP,"-",8*slot1))]  

    | Add -> opp_bin "check_overflow_add" e1 e2 slot1 slot2 env funenv arg_count
    | Sub -> opp_bin "check_overflow_sub" e1 e2 slot1 slot2 env funenv arg_count
    | Mult -> opp_bin "check_overflow_mul" e1 e2 slot1 slot2 env funenv arg_count
    | _ ->
      (compile_expr e1 env funenv arg_count)
      @ test_number_instruction
      @ [IMov (RegOffset(RBP,"-",8*slot1),Reg(RAX))]
      @ (compile_expr e2 env funenv arg_count)
      @ test_number_instruction
      @ [IMov (RegOffset(RBP,"-",8*slot2),Reg(RAX))] 
      @ [IMov (Reg(RAX),RegOffset(RBP,"-",8*slot1))]
  

    (*Evaluates args of a function. It recursively compiles the arg and prepares it for function call*)
    (*If arg is from the first 6 arguments of the function, then it uses registers, else it pushes the argument to the stack*)
    and arg_list_evaluator (arg_exp_list : tag expr list) (env : env) (count:int) (funenv : funenv) (arg_count : int)  =
    match arg_exp_list with
    | h::t ->
      if count<6 (*if first 6 arguments, then move argument result to adequate argument register*)
        then (arg_list_evaluator t env (count+1) funenv arg_count) 
              @ compile_expr h env funenv arg_count 
              @ [IMov(List.nth register_arguments count,Reg(RAX))]
        else (arg_list_evaluator t env (count+1) funenv arg_count) (*else push argument result to stack*)
              @ compile_expr h env funenv arg_count 
              @ [IPush(Reg(RAX))]
    | [] -> []

    

    (*Makes binary operation able to call arithmetics errors*)
    and opp_bin 
    (funct : string) (e1: tag expr) (e2 : tag expr) 
    (slot1 : int)(slot2 : int)(env :env)
    (funenv : funenv) (arg_count : int): instruction list =
 
       (compile_expr e1 env funenv arg_count)
       @ test_number_instruction
       @ [IMov (RegOffset(RBP,"-",8*slot1),Reg(RAX))]
       @ (compile_expr e2 env funenv arg_count)
       @ test_number_instruction
       @ [IMov (RegOffset(RBP,"-",8*slot2),Reg(RAX))]
       @ [IPush(Reg(RSI))] @ [IPush(Reg(RDI))] 
       @ [IMov(Reg(RDI),RegOffset(RBP,"-",8*slot1))]
       @ [IMov(Reg(RSI),RegOffset(RBP,"-",8*slot2))]
       @ [ICall(funct)]
       @ [IPop (Reg(RDI));IPop (Reg(RSI))]
       @ [IMov (Reg(RAX),RegOffset(RBP,"-",8*slot1))]
   
    (* and tuple_evaluator (expr_list: tag expr list) (expr_count : int) (tag:tag) (accum : int) (env : env) 
    (funenv : funenv) (arg_count : int) (list_env_slot :int list) =
    (* printf "List length = %i" (List.length list_env_slot); *)
    match expr_list with
          | _::t -> 
            let slot=(List.nth list_env_slot (accum-1)) in (*slot en cual recupear el resultado de la compilación de un elemento de la tupla*)
            (* printf "slot = %i" (slot); *)
            (getIMov (getReg(getRAX)) (getRegOffset getRBP  "-" (8*slot)))
            @(getIMov (getRegOffset getR15 "+" (accum*8)) (getReg getRAX)) 
            @ (tuple_evaluator t expr_count tag (accum+1) env funenv arg_count list_env_slot)
          | [] -> getIMov (getReg getRAX) (getReg getR15) @
                  getIAdd (getReg getRAX) (getConst 1L) @ (*Se taggea la tupla*)
                  getIAdd (getReg getR15) (getConst (Int64.of_int (8*(expr_count+1)))) Bump del header pointer     *)
    

    (* and tuple_save_evaluator (expr_list: tag expr list) (expr_count : int) (tag:tag) (*esta funcion guarda cada elemento de la tupla en el stack*)
    (accum : int) (env : env) (funenv : funenv) (arg_count : int) (list_env_slot :int list) =
    printf "save List length = %i\n" (List.length list_env_slot);
    printf "save accum = %i\n" accum; 
     match expr_list with
    | h::t -> let slot=(List.nth list_env_slot accum) in (*slot en cual guardar la compilación de un elemento de la tupla*)
      (* printf "save slot = %i\n" (slot);  *)
      (compile_expr h env funenv arg_count) 
    @ (getIMov (getRegOffset (getRBP) "-" (8*slot))(getReg(getRAX))) 
    @ (tuple_save_evaluator t expr_count tag (accum+1) env funenv arg_count list_env_slot)
    | [] -> [] *)
    and generate_list_env_slot (expr_list:tag expr list) (env:env) : ((env*int) list) =
        match expr_list with
            |_::t ->  let (env',slot1) = add (sprintf "tuple") env LocalKind in
                    [(env',slot1)] @ (generate_list_env_slot t env')
            |[] -> []
          

            (* [IMov (Reg(RAX),RegOffset(RBP,"-",8*slot1))] *)
    and compile_tuple (env_slot_list : (env*int) list)(pos : int) (number_of_elements : int) =
    (* printf "pos %i\n" pos;
    printf "number of tuples %i\n" number_of_elements; *)
    match env_slot_list with
      | _::t ->let slot = snd (List.nth env_slot_list 0) in
              (getIMov(getReg getRAX) (getRegOffset getRBP "-" (8*slot))) (*Recuperamos el valor del stack y lo dejamos en RAX*)
              @ (getIMov (getRegOffset (getR15) "+"  ((8*(pos+1)))) (getReg getRAX)) (*Lo metemos al Heap pointer*)
              @ (compile_tuple t (pos+1) number_of_elements)
      | [] ->  getIMov (getReg getRAX) (getReg getR15) @ (*Guardamos el heap pointer en RAX para devolverlo*)
               getIAdd (getReg getRAX) (getConst 1L) @ (*Se taggea la tupla*)
               getIAdd (getReg getR15) (getConst (Int64.of_int (8*(number_of_elements+1)))) (*Bump del header pointer*)   
               
    (* and add_exprlist_to_stack (expr_list : tag expr list) (expr_count : int) (env:env) (funenv:funenv) (arg_count:int) (list_env_slot: (env*int) list) (accum:int) =
    let slot = (snd (List.nth list_env_slot accum)) in
    let env' = (fst (List.nth list_env_slot (expr_count-1))) in
    match expr_list with
        | h:: t -> (compile_expr h env' funenv arg_count) @ (getIMov (getRegOffset (getRBP) "-" (8*slot)) (getReg getRAX))  
                  @ (add_exprlist_to_stack t expr_count env' funenv arg_count list_env_slot (accum+1))
        | [] -> [] *)

    (* and eTuple (expr_list: tag expr list) (expr_count : int) (tag:tag) (accum : int) (env : env) (funenv : funenv) (arg_count : int) =
    
    let list_env_slot= (generate_list_env_slot expr_list env) in
      (add_exprlist_to_stack expr_list expr_count env  funenv arg_count list_env_slot 0) @ 
      (* getIMov (getRegOffset (getReg getR15) "+" (getConst 0L) 0)    @  *)
      (compile_tuple expr_list env tag fuenv tuple_count) *)
    
    (* let env` = List.nth list_env_slot (expr_count -1)
    let evaluated_saved_tuples =(tuple_save_evaluator expr_list expr_count tag 0 env funenv arg_count list_env_slot) in (*Evalua y guarda cada parte de la tupla en el stack*) 
    let evaluated_tuples = (tuple_evaluator expr_list expr_count tag accum env funenv arg_count list_env_slot) in
    evaluated_saved_tuples @ evaluated_tuples *)

(*Adds a list of variables to env*)
let rec add_list (list_variables : string list) (env : env) (kind : kind) =
   match list_variables with
   | h::t -> let (new_env,_) = add h env kind in 
                add_list t new_env kind
   | [] -> env

(* Counts number of local variables in a expression*)
let rec var_count(ex: tag expr) : int  =
  match ex with 
  | Prim1(_,e,_) -> 1 + var_count e
  | Prim2(_,e1,e2,_) -> 1 + max (var_count e1) (var_count e2)
  | Let(_ ,e,body,_) -> 1 + max (var_count e) (var_count body)
  | If(cond,tbranch,fbranch,_)-> 1 + max (var_count cond) (max (var_count tbranch) (var_count fbranch)) (* Arreglar pq cond (max del max)también puede*)
  | Apply (_, e, _) -> 1 + (List.fold_left max 0 (List.map var_count e)) (*agregar max de la lista antes del fold*)
  |_ -> 1


(* Compiles a fun definition/declaration*)
(* It adds the function arguments to env, fun id and arg_count to funenv, and returns new fun environment*)
let compile_decl (fdef:fundef) (fun_env : funenv) : (string * funenv) =
  match fdef with
  | DefFun (id,arg_list,expr) -> 
    let arg_count =  (List.length arg_list) in
    let new_env = add_list arg_list [] ArgKind in
    let new_fun_env = add_fun id arg_count fun_env in
    let count_of_var = Int64.of_int (16* 16 * (var_count expr)) in
    let decl = asm_to_string([ILabel(id);IPush(Reg(RBP));IMov(Reg(RBP),Reg(RSP));ISub(Reg(RSP),Const(count_of_var))] 
              @ (compile_expr (tag expr) new_env new_fun_env arg_count) @ [IMov(Reg(RSP),Reg(RBP))] 
              @ [IPop (Reg(RBP))] @ [IRet]) in
    (decl,new_fun_env)
  | _ -> failwith("Error: DefFun constructor expected in compilation of functions")
  

(* Compiles a list of fun definitions/declarations recursively*)
(* Returns a string of the fun list compilation and the fun environment*)
let rec compile_list_fundef (f_list:fundef list) (fun_env : funenv) : (string * funenv) =
  match f_list with
  | h::t -> 
    let (head,new_fenv) = compile_decl h fun_env in
    let (rest,final_fenv) = compile_list_fundef t new_fenv in
    ("\n"^ head ^ rest,final_fenv)
  | [] -> ("",fun_env) 

let extern_list = 
  ["error";"print";"check_overflow_add";"check_overflow_sub";"check_overflow_mul";"check_non_zero_denominator"]


let rec extern_functions (s:string list): string =
  match s with 
  | [] -> ""
  | h::t -> (sprintf "extern %s\n" h) ^ (extern_functions t)


let error_functions ="
error_not_number:
  mov RSI,RAX
  mov RDI,0x1
  call error
  
error_not_boolean:
  mov RSI,RAX
  mov RDI,0x2
  call error
"

let prologue ="  mov RSP, RBP
  pop RBP
"


  (* "mov R15, RDI
   add R15, 7
   mov R11, 0xfffffffffffffff8L
   and R15, R11
  " *)
  
(*Heap initializer ensures that provided address is multiple of 8*)
let heap_initializer =
  asm_to_string ((getIMov (getReg getR15) (getReg getRDI)) @ 
  (getIAdd (getReg getR15) (getConst 7L)) @
  (getIMov (getReg getR11) (getConst  0xfffffffffffffff8L)) @ 
  (getIAnd (getReg getR15) (getReg getR11)))
  

let prelude(vars: int): string =
  sprintf "section .text
global our_code_starts_here\n" ^ 
(extern_functions extern_list) ^
sprintf "
our_code_starts_here:
  push RBP
  mov RBP, RSP
  sub RSP, 0x%x\n" vars
  
(*Compiles whole program*)
let compile_prog (p:prog)  : string =
  let flist, e = p in
  let (functions,funenv) = compile_list_fundef flist [] in
  let instrs = compile_expr (tag e) [] funenv 0 in
  let vars = 16* 16 * (var_count e) in

  (prelude vars) ^ heap_initializer ^ asm_to_string (instrs) ^ 
   prologue ^ asm_to_string([IRet]) ^ functions ^ error_functions



