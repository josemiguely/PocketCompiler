open Printf

(* registers *)
type reg =
| RAX (* the register where we place answers*)
| RSP (* the stack pointer, below which we can use memory*)

(* arguments for instructions *)
type arg =
| Const of int64 (* explicit numeric constants *)
| Reg of reg (* any named register*)
| RegOffset of reg * int (*RegOffset(reg,i) represents adress [reg + 8*i]*)


(* asm instructions *)
(* type instruction =
| IRet *)
(* | IMov of arg * arg *)
(* TO BE COMPLETED *)


type instruction =
  | IMov of arg * arg (* Move the value of the right-side arg into the left-arg*)
  | IAdd of arg * arg (*Increment the left-hand arg by the value of the right-hand*)
  | IMult of arg * arg (*Multiply the left-hand arg by the value of the right hand*)
  | ICmp of arg * arg (*Compares both args and sets flags*)
  | IJe of string (*Moves execution flow to string label if equal in cmp instruction*)
  | IJmp of string (* Moves execution flow to string label*)
  | ILabel of string (*Section of code*)
  | IRet (*Return*)


let pp_reg reg : string =
  match reg with
  | RAX -> "RAX"
  | RSP -> "RSP"

let pp_arg arg : string =
  match arg with
  | Const n ->  sprintf "%#Lx" n
  | Reg r -> pp_reg r
  | RegOffset (reg,slot) -> sprintf "[%s - 8*%s]" (pp_reg reg) (string_of_int slot)


let rec asm_to_string (asm : instruction list) : string =
  (*do something to get a string of assembly*)
  match asm with
  | [] -> ""
  | [IMov (arg1,arg2)] -> sprintf "  mov %s, %s\n" (pp_arg arg1) (pp_arg arg2)
  | [IAdd (arg1,arg2)]-> sprintf "  add %s, %s\n" (pp_arg arg1) (pp_arg arg2)
  | [ICmp (arg1,arg2)] -> sprintf "  cmp %s, %s\n" (pp_arg arg1) (pp_arg arg2)
  | [IJe (arg1)] -> sprintf "  je %s\n" (arg1)
  | [IJmp (arg1)] -> sprintf "  jmp %s\n" (arg1)
  | [ILabel (arg1)] -> sprintf " %s:\n" (arg1)
  | [IRet] -> sprintf " ret"
  | h :: t -> (asm_to_string [h]) ^ (asm_to_string t)



(* let pp_instr instr : string =
  match instr with
  | IRet -> "  ret" 
  | IMov (a1, a2) -> sprintf "  mov %s, %s" (pp_arg a1) (pp_arg a2) *)
  (* TO BE COMPLETED *)

(* let pp_instrs (instrs : instruction list) : string =
  List.fold_left (fun res i -> res ^ "\n" ^ (pp_instr i)) "" instrs *)


