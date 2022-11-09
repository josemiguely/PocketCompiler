section .text                       
global our_code_starts_here
extern error
extern print
extern check_overflow_add
extern check_overflow_sub
extern check_overflow_mul
extern check_non_zero_denominator
extern tuple_index_error

our_code_starts_here:
  push RBP
  mov RBP, RSP
  sub RSP, 0x100
  mov R15, RDI
  add R15, 0x7
  mov R11, 0xfffffffffffffff8
  and R15, R11
  jmp lambda_id_1_end
  lambda_id_1:
  push RBP
  mov RBP, RSP
  sub RSP, 0
  mov R11, RDI
  sub R11, 0x5
  sub RSP, 0x200
  mov RAX, 0x4
  test RAX, 0x1
  jnz error_not_number
  mov [RBP - 24], RAX
  mov RAX, 0x8
  test RAX, 0x1
  jnz error_not_number
  mov [RBP - 32], RAX
  push RSI
  push RDI
  mov RDI, [RBP - 24]
  mov RSI, [RBP - 32]
  call check_overflow_add
  pop RDI
  pop RSI
  mov RAX, [RBP - 24]
  add RAX, [RBP - 32]
  pop RBP
  ret
  lambda_id_1_end:
  mov R10, [R15 + 0]
  mov R10, 0x1
  mov R10, [R15 + 8]
  mov R10, lambda_id_1
  mov RAX, R15
  add RAX, 0x5
  add R15, 0x18
  mov RSP, RBP
  pop RBP
  ret

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