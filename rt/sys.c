#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>
#include <stdlib.h>

typedef uint64_t u64;

extern u64 our_code_starts_here() asm("our_code_starts_here");

typedef uint64_t VAL;
const uint64_t BOOL_TAG   = 0x0000000000000001;
const VAL BOOL_TRUE =0xffffffffffffffff;
const VAL BOOL_FALSE = 0x7fffffffffffffff; // as chosen in compile.ml

char * safe_type;

VAL print(VAL val) {
  if ((val & BOOL_TAG) == 0) { // val is even ==> number
    printf("> %ld\n", ((int64_t)(val)) / 2); // shift bits right to remove tag
  } else if (val == BOOL_TRUE) {
    printf("> true\n");
  } else if (val == BOOL_FALSE) {
    printf("> false\n");
  } else {
    printf("> Unknown value: %#018x\n", (unsigned int)val); // print unknown val in hex
  }
  return val;
}

VAL print_res(VAL val) {
  if ((val & BOOL_TAG) == 0) { // val is even ==> number
    printf("%ld", ((int64_t)(val)) / 2); // shift bits right to remove tag
  } else if (val == BOOL_TRUE) {
    printf("true");
  } else if (val == BOOL_FALSE) {
    printf("false");
  } else {
    printf("Unknown value: %#018x", (unsigned int)val); // print unknown val in hex
  }
  return val;
}

// void check_overflow (){

//   if(safe_type==1)
//   return 1

//   else return 2
// }

// void check_overflow_add (){

//   if(safe_type==1)...
//   return 1

//   else return 2
// }


//Error codes
const int ERR_NOT_NUMBER=1;
const int ERR_NOT_BOOLEAN=2;

void error(int errCode, VAL val){
 
   if (errCode == ERR_NOT_NUMBER){
    printf("Type error: Expected integer, but got ");
    print_res(val);
    // printf("\n");
    
  }

  else if (errCode == ERR_NOT_BOOLEAN){
    printf("Type error: Expected boolean, but got ");
    print_res(val);
    // printf("\n");
  }
  else{
    printf("Not Boolean or Number, mayor bug in code\n");
  }
  
  exit(errCode);
}


int main(int argc, char** argv) {
  safe_type = argv[0];
  u64 result = our_code_starts_here();
  print_res(result);
  // printf("choriflay\n");
  // printf("%" PRId64 "\n", result);
  return 0;
}
