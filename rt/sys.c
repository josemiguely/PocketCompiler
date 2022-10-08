#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>
#include <stdlib.h>
#include <string.h>

typedef uint64_t u64;

extern u64 our_code_starts_here() asm("our_code_starts_here");

typedef uint64_t VAL;
const uint64_t BOOL_TAG   = 0x0000000000000001;
const VAL BOOL_TRUE =       0xffffffffffffffff;
const VAL BOOL_FALSE =      0x7fffffffffffffff; // as chosen in compile.ml

char safe_type[2];

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
 //printf("%ld\n", ((int64_t)(val)));
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
// operation type



const int64_t MAXINT = INT64_MAX;
const int64_t MININT = INT64_MIN;

// strcmp(s1, s2): Returns 0 if s1 and s2 are the same; less than 0 if s1<s2; greater than 0 if s1>s2.
void check_overflow_add (VAL a, VAL b){

  //printf(" VALOR en OVERFLOW %d\n", *safe_type);
//printf(" a %d\n", a);
//printf(" b %d\n", b);
  //strcpy(safe_type,"2");
  if(*safe_type == '2'){ // 2 en ascii

    if ((int64_t)a/2 > MAXINT/2 - (int64_t)b/2){
      printf("Arithmetic error: + produced an over flow\n");
      exit(-1);
    }
    else if ((int64_t)a/2 < MININT/2 - (int64_t)b/2){
      printf("Arithmetic error: + produced an under flow\n");
      exit(-1);
    }  
    
  }
  

}

void check_overflow_sub (VAL a, VAL b){

  if(*safe_type == '2'){

    if ((int64_t)a/2 > MAXINT/2 + (int64_t)b/2){
      printf("Arithmetic error: - produced an over flow\n");
      exit(-1);
    }
    else if ((int64_t)a/2 < MININT/2 + (int64_t)b/2){
      printf("Arithmetic error: - produced an under flow\n");
      exit(-1);
    }  
  }
  else {}
}

void check_overflow_mul (VAL a, VAL b){
  

  if(*safe_type == '2'){

    if ((int64_t)a/2 > MAXINT/2 / (int64_t)b/2){
      printf("Arithmetic error: * produced an over flow\n");
      exit(-1); // arreglar 
    }
    else if ((int64_t)a/2 < MININT/2 / (int64_t)b/2){
      printf("Arithmetic error: * produced an under flow\n");
      exit(-1);
    } 
    else {}
  }

}

void check_non_zero_denominator (VAL d){

  if(*safe_type == '2'){

    if ((int64_t)d/2 == 0){
      printf("Arithmetic error: Division by 0\n");
      exit(-1);
    }
   // return;
  }
  else {
    //return;
  }
}
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
  
  // printf(" safe type = %s \n",safe_type);
  if(argc > 1){
    strcpy(safe_type,argv[1]);
  }
  u64 result = our_code_starts_here();
  print_res(result);
  // printf("choriflay\n");
  // printf("%" PRId64 "\n", result);
  return 0;
}
