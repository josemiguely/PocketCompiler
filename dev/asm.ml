open Printf

(* registers *)
type reg =
| RAX (* the register where we place answers*)
| RSP (* the stack pointer, below which we can use memory*)
| R10 (* temporary register*)
| RBP (* base pointer*)
| RDI (*arg 1*)
| RSI (*arg 2*)
| RDX (*arg 3*)
| RCX (*arg 4*)
| R8 (*arg 5*)
| R9 (*arg 6*)
| R15 (* heap pointer*)
| R11 (* I think tmp register*)
| R12 (*testing this register*)

(* arguments for instructions *)
type arg =
| Const of int64 (* explicit numeric constants *)
| Reg of reg (* any named register*)
| RegOffset of reg * string * int (*RegOffset(reg,i) represents adress [reg +- 8*i]*)
| ILabelArg of string

type instruction =
  | IMov of arg * arg (* Move the value of the right-side arg into the left-arg*)
  | IAdd of arg * arg (*Increment the left-side arg by the value of the right-side*)
  | ISub of arg*arg (*Decrement the left-side arg by the right-side arg*)
  | IMult of arg * arg (*Multiply the left-side arg by the value of the right-side*)
  | IDiv of arg
  | IAnd of arg * arg
  | IOr of arg * arg
  | ICmp of arg * arg (*Compares both args and sets flags*)
  | IJe of string (*Moves execution flow to string label if equal in cmp instruction*)
  | IJl of string (*Moves execution flow to string label if less than in cmp instruction*)
  | IJne of string (*Moves execution flow to string label is not equal in cmp instruction*)
  | IJmp of string (* Moves execution flow to string label*)
  | IJz of string (*Moves execution flow to string label if zero flag is set*)
  | IJnz of string (*Moves execution flow to string label if not zero flag is set*)
  | IJge of string (*Moves execution flow to string label if greater equl in cmp instruction*)
  | ITest of arg * arg(*Tests two numbers by a logical and*)
  | IXor of arg * arg
  | ILabel of string (*Section of code*)
  | ICall of string (*Call function with string label*)
  | ICallArg of arg (*Call function with arg label*)
  | IPush of arg (*Push arg into top of stack*)
  | IPop of arg (*Pop from top of stack*)
  | IShl of arg * arg (* << *)
  | ISar of arg * arg   (* >> *)
  | ICqo
  | ILine (*Returnes a \n*)
  | IRet (*Return*)


let pp_reg reg : string =
  match reg with
  | RAX -> "RAX"
  | RSP -> "RSP"
  | R10 -> "R10"
  | RBP -> "RBP"
  | RDI -> "RDI"
  | RSI -> "RSI"
  | RDX -> "RDX"
  | RCX -> "RCX"
  | R8 -> "R8"
  | R9 -> "R9"
  | R15 -> "R15"
  | R11 -> "R11"
  | R12 -> "R12"

let pp_arg arg : string =
  match arg with
  | Const n ->  sprintf "%#Lx" n
  | Reg r -> pp_reg r
  | RegOffset (reg,operation,slot) -> sprintf "[%s %s %s]" (pp_reg reg) (operation) (string_of_int slot)
  | ILabelArg (id) -> sprintf "%s" (id)

(** Transforms ASM instruction list to string*)
let rec asm_to_string (asm : instruction list) : string =
  match asm with
  | [] -> ""
  | [IMov (arg1,arg2)] -> sprintf "  mov %s, %s\n" (pp_arg arg1) (pp_arg arg2)
  | [IAdd (arg1,arg2)]-> sprintf "  add %s, %s\n" (pp_arg arg1) (pp_arg arg2)
  | [ISub (arg1,arg2)] -> sprintf "  sub %s, %s\n" (pp_arg arg1) (pp_arg arg2)
  | [ICmp (arg1,arg2)] -> sprintf "  cmp %s, %s\n" (pp_arg arg1) (pp_arg arg2)
  | [IAnd (arg1,arg2)] -> sprintf "  and %s, %s\n" (pp_arg arg1) (pp_arg arg2)
  | [IOr (arg1,arg2)] -> sprintf "  or %s, %s\n" (pp_arg arg1) (pp_arg arg2)
  | [IMult (arg1,arg2)] -> sprintf "  imul %s, %s\n" (pp_arg arg1) (pp_arg arg2)
  | [IDiv (arg1)] -> sprintf "  idiv %s\n" (pp_arg arg1)
  | [IXor (arg1,arg2)] -> sprintf "  xor %s, %s\n" (pp_arg arg1) (pp_arg arg2)
  | [IJe (arg1)] -> sprintf "  je %s\n" (arg1)
  | [IJne (arg1)] -> sprintf "  jne %s\n" (arg1)
  | [IJl (arg1)] -> sprintf "  jl %s\n" (arg1)
  | [IJz (arg1)] -> sprintf "  jz %s\n" (arg1)
  | [IJnz (arg1)] -> sprintf "  jnz %s\n" (arg1)
  | [IJmp (arg1)] -> sprintf "  jmp %s\n" (arg1)
  | [IJge (arg1)] -> sprintf "  jge %s\n" (arg1)
  | [ILabel (arg1)] -> sprintf "  %s:\n" (arg1)
  | [ITest (arg1,arg2)] -> sprintf "  test %s, %s\n" (pp_arg arg1) (pp_arg arg2)
  | [ICall (arg1)] -> sprintf "  call %s\n" (arg1)
  | [ICallArg (arg1)] -> sprintf "  call %s\n" (pp_arg arg1)
  | [IPush (arg1)] -> sprintf "  push %s\n" (pp_arg arg1)
  | [IPop (arg1)] -> sprintf "  pop %s\n" (pp_arg arg1)
  | [IRet] -> sprintf "  ret\n"
  | [ICqo] -> sprintf "  cqo\n"
  | [ILine] -> sprintf "  \n"
  | [IShl (arg1,arg2)] -> sprintf "  shl %s, %s\n" (pp_arg arg1) (pp_arg arg2)
  | [ISar (arg1,arg2)] -> sprintf "  sar %s, %s\n" (pp_arg arg1) (pp_arg arg2)
  | h :: t -> (asm_to_string [h]) ^ (asm_to_string t)




