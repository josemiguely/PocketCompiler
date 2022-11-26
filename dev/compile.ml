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

let test_tuple = 0x0000000000000001L

let save_register_arguments_before_call = 
[IPush(Reg(R9));IPush(Reg(R8));IPush(Reg(RCX));IPush(Reg(RDX));IPush(Reg(RSI));IPush(Reg(RDI))]

let pop_arguments_after_call = 
[IPop(Reg(RDI));IPop(Reg(RSI));IPop(Reg(RDX));IPop(Reg(RCX));IPop(Reg(R8));IPop(Reg(R9))]

let register_arguments = [Reg(RDI);Reg(RSI);Reg(RDX);Reg(RCX);Reg(R8);Reg(R9)]


type kind = 
  | ArgKind
  | LocalKind

type env = (string * int * kind) list


(*Pure functions for registers*)

let getRAX = RAX

let getRSP = RSP

let getR10 = R10

let getR12 = R12

let getRBP = RBP

let getRDI = RDI

let getRSI = RSI

let getRDX = RDX

let getRCX = RCX

let getR8 = R8

let getR9 = R9


let getR15 = R15

let getR11 = R11


(*Pure functions for arguments*)

(**Recieves num of type int64 and returns Const(num) of type arg*)
let getConst ( num:int64 ) : arg =
  Const(num);;

(**Recieves reg1 of type reg. Returns [Reg(reg1)] of type arg*)
let getReg (reg1:reg) : arg =
Reg(reg1)

(** Recieves a register that with a operation will offset an amount. Returns [[RegOffset(reg,op,offset)]]*)
let getRegOffset (reg:reg) (op : string) (offset: int) : arg =
  RegOffset(reg,op,offset)



(*Pure functions for instructions*)

(**Moves second arg to first arg. Returns [[IMov (arg1,arg2)]]*)
let getIMov (arg1 : arg) (arg2: arg) : instruction list =
  [IMov(arg1,arg2)]

(**Add first arg with second arg. Returns [[IAdd (arg1,arg2)]]*)
let getIAdd (arg1 : arg) (arg2 : arg) =
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
  [IJne(label)]


let getIJmp (label : string ) =
  [IJmp(label)]


let getIJz (label : string ) =
  [IJz(label)]


let getIJnz (label : string ) =
  [IJnz(label)]

let getIJge (label : string) =
  [IJge(label)]

let getITest (arg1 : arg) (arg2 : arg) =
  [ITest(arg1,arg2)]

let getIXor (arg1 : arg) (arg2 : arg)=
  [IXor(arg1,arg2)]

let getILabel (label : string ) =
  [ILabel(label)]

let getICall (label : string) =
  [ICall (label)]
  
let getICallArg (label : arg) =
  [ICallArg (label)]

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


let getILine = [ILine]

(*Revisar esto*)
let getILabelArg (id:string) = ILabelArg (id)


let getOr (arg1: arg) (arg2: arg) = 
  [IOr (arg1,arg2)]

let test_number_instruction = [ITest (Reg(RAX),Const(test_number))] @ [IJnz("error_not_number")]

let test_boolean_instruction = [ITest (Reg(RAX),Const(test_number))] @ [IJz("error_not_boolean")]

let test_tuple_instruction =    (getIMov (getReg getRCX) (getReg getRAX))  
                              @ (getIAnd (getReg getRCX) (getConst 7L))          
                              @ (getICmp (getReg getRCX) (getConst 1L))
                              @ (getIJne "error_not_tuple")

let test_closure_instruction =  (getIMov (getReg getR10) (getReg getRAX))  
                              @ (getIAnd (getReg getR10) (getConst 7L))          
                              @ (getICmp (getReg getR10) (getConst 5L))
                              @ (getIJne "error_not_clousure")
let test_index_out_of_bounds =  (getICmp (getReg (getRAX))(getConst 0L) ) (* Si se quiere acceder a una posicion menor que 0 entonces tirar error *)
                              @ (getIJl ("error_tuple_index_error"))
                              @ (getICmp (getReg (getRAX)) (getRegOffset getR11 "+" 0)) (*Si se quiere acceder a un indice mas grande tira error *)
                              @ (getIJge ("error_tuple_index_error"))

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
(* printf "arg_count = %i" arg_count;
printf "track_count = %i" track_count; *)
if track_count == arg_count
  then []
else 
  if track_count<6 then  save_arguments_before_call arg_count (track_count+1) @ [IPush(List.nth register_arguments (track_count))] 
  else [IMov(Reg(RAX),RegOffset(RBP,"+",8))] @ [IPush (Reg(RAX))]

 

(*Restore arguments after calling a new function*)
let rec restore_arguments_after_call (arg_count : int) (track_count : int) : instruction list  =
if (track_count == arg_count || track_count>=6)
    then []
  else 
  [IPop(List.nth register_arguments (track_count))] @ restore_arguments_after_call arg_count (track_count+1)
  
let call_function_one_argument (funct: string) : instruction list = 
  (* [IPush(Reg(RDI))] @[IMov(Reg(RDI),Reg(RAX))] @ [ICall(funct)] @ [IPop (Reg(RDI))] *)
  [IPush(Reg(RDI))] @[IMov(Reg(RDI),Reg(RAX))] @ [ICall(funct)] @ [IPop (Reg(RDI))]

let call_function_two_argument (funct: string) : instruction list = 
  [IPush(Reg(RSI));IPush(Reg(RDI))] @[IMov(Reg(RDI),Reg(RAX))] @ [ICall(funct)] @ [IPop (Reg(RDI));IPop (Reg(RSI))] 


(**Adds a list of variables to env*)
let rec add_list (list_variables : string list) (env : env) (kind : kind) =
  match list_variables with
  | h::t -> let (new_env,_) = add h env kind in 
               add_list t new_env kind
  | [] -> env

(** Counts number of local variables in a expression*)
let rec var_count(ex: tag expr) : int  =
  match ex with 
  | Prim1(_,e,_) -> 1 + var_count e
  | Prim2(_,e1,e2,_) -> 1 + max (var_count e1) (var_count e2)
  | Let(_ ,e,body,_) -> 1 + max (var_count e) (var_count body)
  | If(cond,tbranch,fbranch,_)-> 1 + max (var_count cond) (max (var_count tbranch) (var_count fbranch)) (* Arreglar pq cond (max del max)también puede*)
  | Apply (_, e, _) -> 1 + (List.fold_left max 0 (List.map var_count e)) (*agregar max de la lista antes del fold*)
  |_ -> 1

(**Recibe una expresion y una lista de variables en scope, devuelve una lista de variables libres*)
let rec freeVars (expr : tag expr) (vars_in_scope : string list) (free_vars_list : string list) : string list =
  let uniq_cons x xs = if List.mem x xs then xs else x :: xs in
  match expr with
  | Prim1 (_,expr,_) -> (freeVars expr vars_in_scope free_vars_list)
  | Let (x,e,b,_) ->
    let free_vars_expr = (freeVars e vars_in_scope free_vars_list) in 
    let vars_in_scope = [x] @ vars_in_scope in
                (freeVars b vars_in_scope free_vars_expr)
  | Id (x,_) -> let is_in_scope=(List.mem x vars_in_scope) in
      let is_in_free_vars=(List.mem x free_vars_list) in
        if (is_in_scope || is_in_free_vars) then free_vars_list 
        else [x] @ free_vars_list
  | Prim2 (_,expr1,expr2,_) -> let free_vars_left =(freeVars expr1 vars_in_scope free_vars_list) in 
    (freeVars expr2 vars_in_scope free_vars_left) 
  | Apply (_,expr_list,_) -> 
        let free_vars_expr_list = (freeVars_in_list expr_list vars_in_scope free_vars_list) in
        (List.fold_right uniq_cons free_vars_expr_list [])
  | Set (t,k,v,_) -> let free_vars_t =(freeVars t vars_in_scope free_vars_list) in 
    let free_vars_k =(freeVars k vars_in_scope free_vars_t) in 
    (freeVars v vars_in_scope free_vars_k) 
  | Tuple (expr_list,_) -> 
      let free_vars_expr_list = (freeVars_in_list expr_list vars_in_scope free_vars_list) in
      (List.fold_right uniq_cons free_vars_expr_list [])
  | Lambda (id_list,body,_) -> 
    let vars_in_scope = id_list @ vars_in_scope in
    freeVars body vars_in_scope free_vars_list
  | LamApply (lambda_expr,arg_list,_) -> 
      (*let remove_from_right = List.fold_right uniq_cons ["x";"y";"z";"a";"x";"y";"z"] []*)
      let free_vars_lambda =(freeVars lambda_expr vars_in_scope free_vars_list) in 
      let free_vars_arg_list = (freeVars_in_list arg_list vars_in_scope free_vars_lambda) in
      (List.fold_right uniq_cons free_vars_arg_list [])

  | _ -> free_vars_list (*Solamente deberia aplicar a Num y Bools*)
  

  and freeVars_in_list (expr_list : tag expr list) (vars_in_scope : string list) (free_vars_list : string list ) =
  match expr_list with
    | h::t -> (freeVars_in_list t vars_in_scope free_vars_list) @ freeVars h vars_in_scope free_vars_list 
    | []  -> []


(**Devuelve los primeros amount slots de un env*)
  let rec get_slots (env : env) (amount : int) : int list =
    if (amount>0) then
      match env with
      | (_,slot,_) :: t -> [slot] @ get_slots t (amount -1)
      | [] -> failwith("Error, se intento obtener mas slots de los que existen en el ambiente")
    else []


  (*Carga en el stack las free-vars desde la clausura. Las coloca en la posición correcta en el stack usando slot_list*)
  let rec load_free_vars_to_stack (slot_list : int list) (accum : int) : instruction list =
    match slot_list with
    | h::t -> let slot = h in (*Slot correspondiente*)
    getIMov (getReg getRAX) (getRegOffset getR11 "+" accum) (*Cargo un freevars desde el offset*)
    @ getIMov (getRegOffset getRBP "-" (8*slot)) (getReg getRAX) (*Lo meto a la posición de stack correcta*)
    @ load_free_vars_to_stack t (accum+8) (*sigo con la siguiente free vars*)
    | [] -> []
    

(**Returns value of a id symbol in an env*)
let find_id_value (x : string) (env : env) : instruction list = 
  let (slot,kind) = (lookup x env) in
    (match kind with
      | LocalKind -> [IMov (Reg(RAX),RegOffset(RBP,"-",8*slot))]
      | ArgKind  ->(
          if slot<=6 then [IMov (Reg(RAX),List.nth register_arguments (slot-1))]
          else [IMov (Reg(RAX),RegOffset(RBP,"+",8*(slot-7+2)))]))

let rec add_free_vars_to_closure (free_vars_list : string list) (env : env) (accum : int) : instruction list =
  match free_vars_list with
    | h::t -> (find_id_value h env) @ (getIMov (getRegOffset getR15 "+" accum) (getReg getRAX)) @ add_free_vars_to_closure t env (accum+8)
    | [] -> []
  
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
      (match e with
        | _ ->
        (*Compile the binding, and get the result into RAX*)
        (compile_expr e env funenv arg_count)
        (*Si es lambda entonces agregar getILabel("x")*)
        (* Copy the result in RAX into the appropriate stack slot*)
        @ [IMov (RegOffset(RBP,"-",8*slot),Reg(RAX))]
        (* Compile the body, given that x is in the correct slot when it's needed*)
        @ (compile_expr b env' funenv arg_count)
        )
      
    | Id (x,_) -> find_id_value x env 
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
        | Or -> scaffold
        | Lt -> (
          let less_label = sprintf "less_%d" tag in
          scaffold 
          @ (eLt slot2 less_label))
        | Get -> 
                (compile_expr expr1 env funenv arg_count) (*Compilo la tupla *)
                 @ test_tuple_instruction (*Checkeo si en RAX hay una tupla *)
                @ (getISub (getReg getRAX) (getConst (1L))) (*Le saco el tag a la tupla*)
                @ (getIMov (getReg getR11) (getReg getRAX) ) (*Dejo el resultado de la tupla en R11*)
                @ (getIPush (getReg getR11)) (*Pusheo la tupla en R11*)
                @ (compile_expr expr2 env funenv arg_count) (*Compilo la posicion n y falta ver que es valida*) 
                @ (getIPop (getReg getR11)) (*Recupero la tupla*)
                @ (getISar (getReg getRAX) (getConst 1L) )(*Se divide el numero de la posicion en 2*)
                @ test_index_out_of_bounds
                @ (getIAdd (getReg getRAX) (getConst 1L))
                @ (getIMult (getReg getRAX) (getConst 8L)) (*Lo multiplico por 8*)
                @ (getIAdd (getReg getR11) (getReg getRAX)) (*Le sumo el resultado de del tamano n para despues desreferenciarlo*)
                @ (getIMov (getReg getRAX) (getRegOffset getR11 "+" 0))           
        | _ -> failwith("Unexpected binary operation") ) 

  | Apply(id,expr_list,_) -> 
    let instr = arg_list_evaluator expr_list env 0 funenv arg_count in (*First we eval Apply arguments*)
    let arg_number = List.length expr_list in
    let _ = lookup_fun id funenv arg_number in
    let arg_more_than_6_offset = (arg_number-6)*8 in
    let res = Int64.of_int (max arg_more_than_6_offset 0) in
    save_arguments_before_call arg_count 0 (* saves previous function arguments*)
    @ instr (* Evaluates arguments and inserts arguments for function call*)
    @ [ICall(id)]  
    @ [IAdd(Reg(RSP),Const(res))] 
    @ restore_arguments_after_call arg_count 0

  | Set (t,k,v,_) -> 
     (compile_expr t env funenv arg_count)     
    @ test_tuple_instruction  (*testeo de que en RAX se tenga una tupla*)
    @ (getISub (getReg getRAX) (getConst 1L))     (*Quitamos la tag tag de tupla*)
    @ (getIMov (getReg getR11) (getReg getRAX) ) (* Dejo el resultado de la tupla en R11*)
    @ (getIPush (getReg getR11))                 (*Pusheo la tupla en R11*)
    @ (compile_expr k env funenv arg_count)     (* RAX tiene a k *)
    @ getIPop (getReg getR11) 
    @ (getISar (getReg getRAX) (getConst 1L) ) 
    @ test_index_out_of_bounds
    @ (getIAdd (getReg getRAX) (getConst 1L)) 
    @ (getIMov (getReg getR10) (getReg getRAX))
    @ (getIPush (getReg getR11)) 
    @ (getIPush (getReg getR10))  (*R11 tiene la tupla , R10 k*)
    @ (compile_expr v env funenv arg_count) 
    @ (getIMov (getReg getRDX) (getReg getRAX)) (* guardo el nuevo valor V en RDX  *) 
    @ (getIPop (getReg getR10))               
    @ (getIPop (getReg getR11)) 
    @ (getIMov (getReg getRAX) (getReg getR11))  
    @ (getIMult (getReg getR10) (getConst 8L))  (* k*8 *)
    @ (getIAdd (getReg getR11) (getReg getR10))   (* tupla sin tag + k*8   *) 
    @ (getIMov (getRegOffset getR11 "+" 0) (getReg getRDX))  (* [tupla + k*8] <- V *)
    @ (getIAdd (getReg getRAX) (getConst 1L) )
     
  | Tuple(expr_list,_) -> 
    (*Nmero de expresiones en la tupla*)
    let expr_number = List.length expr_list in
    if expr_number==0 then (*Caso cuando la tupla es vacia*)
        
        
        (*llamado a try_gc*)
        getIPush (getReg getR9)
        @ getIPush (getReg getR8)
        @ getIPush (getReg getRCX)
        @ getIPush (getReg getRDX)
        @ getIPush (getReg getRSI)
        @ getIPush (getReg getRDI)
        @ getIMov (getReg getRDI) (getReg getR15) (*Se coloca alloc ptr*)
        @ getIMov (getReg getRSI) (getConst 1L) (*Se coloca espacio solicitado, para la tupla vacia solo 1 espacio, el numero de elementos que es 0*)
        @ getIMov (getReg getRDX) (getReg getRBP) (*Se coloca base pointer*)
        @ getIMov (getReg getRCX) (getReg getRSP) (*Se coloca stack pointer*)
        @ getICall ("try_gc") 
        @ getIPop (getReg getRDI)
        @ getIPop (getReg getRSI)
        @ getIPop (getReg getRDX)
        @ getIPop (getReg getRCX)
        @ getIPush (getReg getR8)
        @ getIPush (getReg getR9)
        
        (*Coloco nuevo puntero de try_gc a R15*)
        @ getIMov (getReg getR15) (getReg getRAX)

        (*fin de llamado a try_gc*)

        @ getIMov (getReg (getR10)) (getConst (Int64.of_int expr_number))
        @ getIMov (getRegOffset (getR15) "+"  ((8*0))) (getReg getR10) 
        @ (add_tuple_to_heap [] 0 expr_number)
    
    else (*Cuando la tupla no es vacia*)

      let env_slot =(generate_list_env_slot expr_list env) in
      let finalenv = (fst (List.nth env_slot  (expr_number-1))) in
      let compiled = 
        (List.map2 ( fun x y -> 
          let res = compile_expr x finalenv funenv arg_count in
          let slot = (snd y) in 
          res @ (getIMov (getRegOffset (getRBP) "-" (8*slot))(getReg(getRAX)))) expr_list env_slot ) in
      let compiled_folded = (List.fold_left (fun x y-> x @ y) [] compiled) in
      compiled_folded 
      
      
      
      (*llamado a try_gc*)
      @ getIPush (getReg getR9)
      @ getIPush (getReg getR8)
      @ getIPush (getReg getRCX)
      @ getIPush (getReg getRDX)
      @ getIPush (getReg getRSI)
      @ getIPush (getReg getRDI)
      @ getIMov (getReg getRDI) (getReg getR15) (*Se coloca alloc ptr*)
      @ getIMov (getReg getRSI) (getConst (Int64.of_int (expr_number + 1))) (*Se coloca espacio solicitado*)
      @ getIMov (getReg getRDX) (getReg getRBP) (*Se coloca base pointer*)
      @ getIMov (getReg getRCX) (getReg getRSP) (*Se coloca stack pointer*)
      @ getICall ("try_gc") 
      @ getIPop (getReg getRDI)
      @ getIPop (getReg getRSI)
      @ getIPop (getReg getRDX)
      @ getIPop (getReg getRCX)
      @ getIPush (getReg getR8)
      @ getIPush (getReg getR9)
      
      (*Coloco nuevo puntero de try_gc a R15*)
      @ getIMov (getReg getR15) (getReg getRAX)

      (*fin de llamado a try_gc*)
      
      
      @ getIMov (getReg (getR10)) (getConst (Int64.of_int expr_number))
      @ getIMov (getRegOffset (getR15) "+"  ((8*0))) (getReg getR10) (*size de la tupla*)
      @ (add_tuple_to_heap env_slot 0 expr_number) (*Añado el resto de la tupla a HeapPointer y devuelvo Pointer*)
  
  | Lambda (id_list,body,tag) -> 
    let arg_count = List.length id_list in (*Cantidad de argumentos del lambda*)
    let free_vars = (freeVars body id_list []) in (* Listado de free_vars*)
    let free_vars_length = List.length free_vars in
    let space_in_stack = Int64.of_int (8 * (free_vars_length)) in (*calculo del espacio que se tiene que anadir en el stack*)
    let self_env , _ = add (sprintf "self_id_%i" tag) [] ArgKind in (*Nuevo ambiente para compilar el cuerpo de lambda, primero solo con el self*)
    let arg_env = add_list id_list self_env ArgKind in (*Nuevo ambiente con el self y los argumentos del lambda*)
    let new_env = add_list free_vars arg_env LocalKind in (*Nuevo ambiente con el self, argumentos del lambda y las freevars*)
    let slots_list = get_slots new_env (free_vars_length) in  (*Obtengo los slots de stack las free-vars añadidas al ambiente*)
    let loading_of_stack = load_free_vars_to_stack slots_list 24  in (*Carga  del stack las free_vars con los slots obtenidos*)
    let count_of_var = Int64.of_int (16* 16 * (var_count body)) in (*Calculo de espacio para variables locales*)
    let add_free_vars_to_closure = (add_free_vars_to_closure free_vars env 24) in
    let space_for_try_gc = 3 + free_vars_length in (* el espacio es : 3 = aridad, code pointer, num var libres + variables libres en total *)
     getIJmp (sprintf "lambda_id_%i_end" tag)
    @ getILabel (sprintf "lambda_id_%i" tag)
    @ getIPush (getReg getRBP) (*comienzo de prologo*)
    @ getIMov (getReg getRBP) (getReg getRSP) (*fin de prologo*)
    @ getISub (getReg getRSP) (getConst space_in_stack) (*Espacio en el stack para closed-over vars*)
    @ getIMov (getReg getR11) (getReg getRDI) (*Cargo el self argument*)
    @ getISub (getReg getR11) (getConst 5L) (*Untag del self*)
    @ loading_of_stack (*Cargo en el stack las free_vars desde la clausura*)
    @ getISub (getReg getRSP) (getConst count_of_var) (*Hago espacio para variables locales*)
    @ compile_expr body new_env funenv (arg_count + 1) (*Compilo el cuerpo del lambda*)
    @ getIMov (getReg getRSP) (getReg getRBP)
    @ getIPop (getReg getRBP)
    @ getIRet
    @ getILabel (sprintf "lambda_id_%i_end" tag)
    
    (*Comienzo de creacion de la clausura*)

    (*llamado a try_gc*)
    @ getIPush (getReg getR9)
    @ getIPush (getReg getR8)
    @ getIPush (getReg getRCX)
    @ getIPush (getReg getRDX)
    @ getIPush (getReg getRSI)
    @ getIPush (getReg getRDI)
    @ getIMov (getReg getRDI) (getReg getR15) (*Se coloca alloc ptr*)
    @ getIMov (getReg getRSI) (getConst (Int64.of_int space_for_try_gc)) (*Se coloca espacio solicitado*)
    @ getIMov (getReg getRDX) (getReg getRBP) (*Se coloca base pointer*)
    @ getIMov (getReg getRCX) (getReg getRSP) (*Se coloca stack pointer*)
    @ getICall ("try_gc") 
    @ getIPop (getReg getRDI)
    @ getIPop (getReg getRSI)
    @ getIPop (getReg getRDX)
    @ getIPop (getReg getRCX)
    @ getIPush (getReg getR8)
    @ getIPush (getReg getR9)
    
    (*Coloco nuevo puntero de try_gc a R15*)
    @ getIMov (getReg getR15) (getReg getRAX)

    (*fin de llamado a try_gc*)



    @ getIMov (getReg getR11) (getConst (Int64.of_int arg_count))
    @ getIMov (getRegOffset getR15 "+" 0) (getReg getR11) (*Colocamos aridad *)
    @ getIMov (getReg getR11) (getILabelArg (sprintf "lambda_id_%i" tag))
    @ getIMov (getRegOffset getR15 "+" 8) (getReg getR11) (*Colocamos el label/code pointer*)
    @ getIMov (getReg getR11) (getConst (Int64.of_int free_vars_length))
    @ getIMov (getRegOffset getR15 "+" 16) (getReg getR11) (*Colocamos la cantidad de variable libres*)
    
    @ add_free_vars_to_closure (*Agregamos las variables libres a la clausura*)
    @ getIMov (getReg getRAX) (getReg getR15) (*Dejo la clausura creada en RAX para asi devolverla*)
    @ getIAdd (getReg getRAX) (getConst 5L) (* Taggeamos la clausura. *)
    @ getIAdd (getReg getR15) (getConst (Int64.of_int (free_vars_length*8+24)))
    (*Fin de creación de la clausura*)

  | LamApply (lambda_expr,arg_list, _ ) -> 
    
    let arg_number = List.length arg_list in
    let instr = arg_list_evaluator arg_list env 1 funenv arg_count in (*First we eval Apply arguments*)
    let arg_more_than_6_offset = (arg_number-6)*8 in
    let res = Int64.of_int (max arg_more_than_6_offset 0) in
    (compile_expr lambda_expr env funenv arg_count) (*Compilo el lambda*)
    @ test_closure_instruction (*checkemos que sea una clausura RAX*)
    @ getIMov (getReg getRDI) (getReg getRAX)
    @ getISub (getReg getRAX) (getConst 5L) (*Le sacamos el tag*)
    @ getIMov(getReg getR11) (getReg getRAX)  (*guardamos en R11 la clausura sin tag*)   

    @ getIMov (getReg getR10) (getRegOffset getR11 "+" 0)  (* R10 tiene la aridad correcta *)
    @ getIMov (getReg getR11) (getConst (Int64.of_int arg_number)) (* R11 tiene la aridad entregada *)
    @ getICmp (getReg getR10) (getReg getR11) (*se comparan las aridades*)
    @ (getIJne "error_arity_mismatch")

    @ getIMov(getReg getR12) (getReg getRAX) (*Volvemos a guardar en R11 la clausura sin tag*)  
    @ save_arguments_before_call arg_count 0 (* saves previous function arguments*)
    @ instr (* Evaluates arguments and inserts arguments for function call*)
    @ getIMov (getReg getR10) (getRegOffset getR12 "+" 8) 
    @ getICallArg (getReg getR10) 
    @ getIAdd (getReg getRSP) (getConst res) (*pop arguments*)
    @ restore_arguments_after_call arg_count 0
                           
  | LetRec (_,_,_) -> failwith("Let Rec not implementd")
  

    
      
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
    | Or -> 
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
   

    and generate_list_env_slot (expr_list:tag expr list) (env:env) : ((env*int) list) =
        match expr_list with
            |_::t ->  let (env',slot1) = add (sprintf "tuple") env LocalKind in
                    [(env',slot1)] @ (generate_list_env_slot t env')
            |[] -> []
          

            
    and add_tuple_to_heap (env_slot_list : (env*int) list)(pos : int) (number_of_elements : int) =
    match env_slot_list with
      | _::t ->let slot = snd (List.nth env_slot_list 0) in
              (getIMov(getReg getRAX) (getRegOffset getRBP "-" (8*slot))) (*Recuperamos el valor del stack y lo dejamos en RAX*)
              @ (getIMov (getRegOffset (getR15) "+"  ((8*(pos+1)))) (getReg getRAX)) (*Lo metemos al Heap pointer*)
              @ (add_tuple_to_heap t (pos+1) number_of_elements)
      | [] ->  getIMov (getReg getRAX) (getReg getR15) @ (*Guardamos el heap pointer en RAX para devolverlo*)
               getIAdd (getReg getRAX) (getConst 1L) @ (*Se taggea la tupla*)
               getIAdd (getReg getR15) (getConst (Int64.of_int (8*(number_of_elements+1)))) (*Bump del header pointer*)   
               
               (* id_list,body,tag *)
   
(* Compiles a declaration*)
(* For functions adds the function arguments to env, fun id and arg_count to funenv, and returns new fun environment*)
let compile_decl (decl: fundef) (fun_env : funenv) : (string * funenv) =
  match decl with
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
let rec compile_list_decl (decl_list:fundef list) (fun_env : funenv) : (string * funenv) =
  match decl_list with
  | h::t -> 
    let (head,new_fenv) = compile_decl h fun_env in
    let (rest,final_fenv) = compile_list_decl t new_fenv in
    ("\n"^ head ^ rest,final_fenv)
  | [] -> ("",fun_env) 

let extern_list = 
  ["error";"print";"check_overflow_add";"check_overflow_sub";"check_overflow_mul";"check_non_zero_denominator";"tuple_index_error";"closure_arity_mismatch";"try_gc";"set_stack_bottom"]


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

error_not_tuple:
  mov RSI,RAX
  mov RDI,0x3
  call error

error_tuple_index_error:
  mov RDI,R11
  mov RSI,RAX
  add R11,0x1
  call tuple_index_error

error_not_clousure:
  mov RSI,RAX
  mov RDI,0x4
  call error

error_arity_mismatch:
  mov RDI,R10
  mov RSI,R11
  call closure_arity_mismatch
"

let prologue ="  mov RSP, RBP
  pop RBP
"
 
(*Heap initializer ensures that provided address is multiple of 8*)
let heap_initializer =
  asm_to_string ((getIMov (getReg getR15) (getReg getRDI)) 
  @ (getIAdd (getReg getR15) (getConst 7L)) 
  @ (getIMov (getReg getR11) (getConst  0xfffffffffffffff8L)) 
  @ (getIAnd (getReg getR15) (getReg getR11))
  @ (getIMov (getReg getRDI) (getReg getRBP))
  @ (getICall ("set_stack_bottom"))
  )
  

let prelude (vars: int): string =
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
  let decl_list, e = p in
  let (functions,funenv) = compile_list_decl decl_list [] in
  let instrs = compile_expr (tag e) [] funenv 0 in
  let vars = 16* 16 * (var_count e) in

  (prelude vars) ^ heap_initializer ^ asm_to_string (instrs) ^ 
   prologue ^ asm_to_string([IRet]) ^ functions ^ error_functions



