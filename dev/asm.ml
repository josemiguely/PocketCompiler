open Printf

(* registers *)
type reg =
| RAX (* the register where we place answers*)
| RSP (* the stack pointer, below which we can use memory*)
| R10 (* temporary register*)
| RBP (* base pointer*)

(* arguments for instructions *)
type arg =
| Const of int64 (* explicit numeric constants *)
| Reg of reg (* any named register*)
| RegOffset of reg * int (*RegOffset(reg,i) represents adress [reg + 8*i]*)

type instruction =
  | IMov of arg * arg (* Move the value of the right-side arg into the left-arg*)
  | IAdd of arg * arg (*Increment the left-hand arg by the value of the right-hand*)
  | IMult of arg * arg (*Multiply the left-hand arg by the value of the right hand*)
  | IAnd of arg * arg
  | ICmp of arg * arg (*Compares both args and sets flags*)
  | IJe of string (*Moves execution flow to string label if equal in cmp instruction*)
  | IJl of string (*Moves execution flow to string label if less than in cmp instruction*)
  | IJne of string (*Moves execution flow to string label is not equal in cmp instruction*)
  | IJmp of string (* Moves execution flow to string label*)
  | ITestNumber (*Test if result is a number*)
  | IXor of arg * arg
  | ILabel of string (*Section of code*)
  | IRet (*Return*)


let pp_reg reg : string =
  match reg with
  | RAX -> "RAX"
  | RSP -> "RSP"
  | R10 -> "R10"
  | RBP -> "RBP"

let pp_arg arg : string =
  match arg with
  | Const n ->  sprintf "%#Lx" n
  | Reg r -> pp_reg r
  | RegOffset (reg,slot) -> sprintf "[%s - 8*%s]" (pp_reg reg) (string_of_int slot)


(** Transforms ASM instruction list to string*)
let rec asm_to_string (asm : instruction list) : string =
  match asm with
  | [] -> ""
  | [IMov (arg1,arg2)] -> sprintf "mov %s, %s\n" (pp_arg arg1) (pp_arg arg2)
  | [IAdd (arg1,arg2)]-> sprintf "add %s, %s\n" (pp_arg arg1) (pp_arg arg2)
  | [ICmp (arg1,arg2)] -> sprintf "cmp %s, %s\n" (pp_arg arg1) (pp_arg arg2)
  | [IAnd (arg1,arg2)] -> sprintf "and %s, %s\n" (pp_arg arg1) (pp_arg arg2)
  | [IXor (arg1,arg2)] -> sprintf "xor %s, %s\n" (pp_arg arg1) (pp_arg arg2)
  | [IJe (arg1)] -> sprintf "je %s\n" (arg1)
  | [IJne (arg1)] -> sprintf "jne %s\n" (arg1)
  | [IJl (arg1)] -> sprintf "jl %s\n" (arg1)
  | [IJmp (arg1)] -> sprintf "jmp %s\n" (arg1)
  | [ILabel (arg1)] -> sprintf "%s:\n" (arg1)
  | [ITestNumber]
  | [IRet] -> sprintf "ret"
  | h :: t -> (asm_to_string [h]) ^ (asm_to_string t)




