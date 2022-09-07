open Ast
open Asm
open Printf

(* let rec compile_expr (e : expr) : instruction list =
  match e with 
  | Num n -> [ IMov (Reg RAX, Const n) ] 
  | _ -> failwith "TO BE DONE!" *)


type env = (string * int) list


let rec lookup name env =
  match env with
  | [] -> failwith (sprintf "Identifier %s is not found in environment" name)
  | (n,i)::rest->
    if n = name then i else (lookup name rest)


    let add name env : (env * int) =
      let slot = 1 + (List.length env) in
      ((name,slot)::env,slot)

  
  
  
  (* let rec asm_to_string (asm : instruction list) : string =
    (*do something to get a string of assembly*)
    match asm with
    | [] -> ""
    | [IMov (arg1,arg2)] -> sprintf "  mov %s, %s\n" (arg_to_string arg1) (arg_to_string arg2)
    | [IAdd (arg1,arg2)]-> sprintf "  add %s, %s\n" (arg_to_string arg1) (arg_to_string arg2)
    | [ICmp (arg1,arg2)] -> sprintf "  cmp %s, %s\n" (arg_to_string arg1) (arg_to_string arg2)
    | [IJe (arg1)] -> sprintf "  je %s\n" (arg1)
    | [IJmp (arg1)] -> sprintf "  jmp %s\n" (arg1)
    | [ILabel (arg1)] -> sprintf " %s:\n" (arg1)
    | [IRet] -> sprintf "  ret"
    | h :: t -> (asm_to_string [h]) ^ (asm_to_string t) *)


    
  let rec compile_expr (e : tag expr) (env : env) : instruction list =
    (* print_string (sprintf "%s\n" (string_of_expr e)); *)
    match e with
    | Num (n,_) ->
      (* let shifted = (Int64.shift_left n 1) in *)
      if n > Int64.max_int || n < Int64.min_int then
        failwith ("Integer overflow: " ^ (Int64.to_string n))
      else
        [IMov (Reg(RAX),Const(n))]
    | Bool (true,_) -> failwith("Boolean t")
    | Bool (false,_) -> failwith("Boolean f")
    | Prim1 (prim1,expr,_) -> (
      match prim1 with 
      | Add1 -> (compile_expr expr env) @ [IAdd (Reg(RAX),Const(1L))]
      | Sub1 -> (compile_expr expr env) @ [IAdd (Reg(RAX),Const(-1L))]
      )
    | Let (x,e,b,_) -> 
      let (env',slot) = add x env in
      (*Compile the binding, and get the result into RAX*)
      (compile_expr e env) 
      (* Copy the result in RAX into the appropriate stack slot*)
      @ [IMov (RegOffset(RSP,1*slot),Reg(RAX))]
      (* Compile the body, given that x is in the correct slot when it's needed*)
      @ (compile_expr b env')
    | Id (x,_) -> let slot = (lookup x env) in
      [IMov (Reg(RAX),RegOffset(RSP,1*slot))]
    | If (cond,thn,els,tag) ->
      let else_label = sprintf "if_false_%d" tag in
      let done_label = sprintf "done_%d" tag in
      (compile_expr cond env) @
      [
       ICmp(Reg(RAX),Const(0L));
       IJe(else_label)
      ]
      @ (compile_expr thn env)
      @ [ IJmp(done_label); ILabel(else_label)]
      @ (compile_expr els env)
      @ [ ILabel (done_label)] 
    | Prim2 (prim2,expr1,expr2,_) -> (
      let (env',slot1) = add "izq" env in
      let (env'',slot2) = add "der" env' in
      let scaffold = (prim2_scaffold expr1 expr2 slot1 slot2 env'') in
      match prim2 with
      | Add ->  scaffold @ [IAdd (Reg(RAX),RegOffset(RSP,slot2))] 
      | _ -> failwith("tonto aun faltan las demás prim2")
  
      )  

    and prim2_scaffold (e1: tag expr) (e2 : tag expr) (slot1 : int)(slot2 : int)(env :env) : instruction list =
    (compile_expr e1 env)
    @ [IMov (RegOffset(RSP,1*slot1),Reg(RAX))]
    @ (compile_expr e2 env)
    @ [IMov (RegOffset(RSP,1*slot2),Reg(RAX))]
    @ [IMov (Reg(RAX),RegOffset(RSP,slot1))]


let compile e : string =
  let instrs = compile_expr e [] in
  let prelude ="
section .text
global our_code_starts_here
our_code_starts_here:\n" in
prelude ^ asm_to_string (instrs @ [ IRet ])




(* Some OCaml boilerplate for reading files and command-line arguments *)
let () =
  (* let input_file = (open_in (Sys.argv.(1))) in *)
  (* let input_program = Int64.of_string (input_line input_file) in *)
  let input_program = "(let (a 10)
  (let (c (let (b (add1 a))
           (let (d (add1 b))
            (add1 b))))
   (add1 c)))" in
   let src = Parse.sexp_from_string input_program in
   let prog = tag (Parse.parse_exp src) in
   (* let prog = Let("a",Num 10L,Let("c",Let("b",Prim1(Add1,Id "a"),Let("d",Prim1(Add1,(Id "b")),Prim1(Add1,Id "b"))),Prim1(Add1,Id "c"))) in *)
  (* close_in input_file; *)
  print_string " Comienza compilación\n";
  let program = (compile prog) in
  print_string " Termina compilación\n";
  printf "%s\n" program;;
  

  