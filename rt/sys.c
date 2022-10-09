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



const int64_t MAXINT = INT64_MAX/2;
const int64_t MININT = INT64_MIN/2;

void check_overflow_add (VAL a, VAL b){

  if(*safe_type == '2'){ // 2 en ascii


    if ((int64_t)a/2 > MAXINT - (int64_t)b/2){
      printf("Arithmetic error: + produced an overflow");
      exit(-1);
    }
    else if ((int64_t)a/2 < MININT - (int64_t)b/2){
      printf("Arithmetic error: + produced an underflow");
      exit(-1);
    }  
    
  }
  

}

void check_overflow_sub (VAL a, VAL b){

  if(*safe_type == '2'){

    
    if ((int64_t)a/2 > MAXINT + (int64_t)b/2){
      printf("Arithmetic error: - produced an overflow");
      exit(-1);
    }
    else if ((int64_t)a/2 < MININT + (int64_t)b/2){
      printf("Arithmetic error: - produced an underflow");
      exit(-1);
    }  
  }
  else {}
}

void check_overflow_mul (VAL a, VAL b){
  
  // a * b = c
  int64_t a2 = ((int64_t)a)/2;
  int64_t b2 = ((int64_t)b)/2;

  if(*safe_type == '2' && b2 !=0 ){

    if (a2 < 0 && b2 < 0 || a2 > 0 && b2 > 0){

   
    if (a2 > MAXINT / b2){
      printf("Arithmetic error: * produced an overflow");
      exit(-1);
    }
    else if (a2 < MININT / b2){
      printf("Arithmetic error: * produced an underflow");
      exit(-1);
    } 
    else{}
    }

  
    
  else{

    //if (a2 > 0)
    //   a2 = a2*-1;

    if (b2 < 0)
      b2 = b2*-1;

      if (a2 < MAXINT / b2){
              printf("Arithmetic error: * produced an overflow");
              exit(-1); 
      }
      else if(a2 > MININT / b2){
        printf("Arithmetic error: * produced an underflow");
        exit(-1);
      } 
      
    else {}}
  }

}

void check_non_zero_denominator (VAL d){

  if(*safe_type == '2'){

    if ((int64_t)d/2 == 0){
      printf("Arithmetic error: Division by 0");
      exit(-1);
    }

  }
  else {

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
    printf("Not Boolean or Number, mayor bug in code");
  }
  
  exit(errCode);
}


int main(int argc, char** argv) {
  
  char *SAFE = getenv("SAFE");
  
  if (SAFE){
     strcpy(safe_type,SAFE);
  }
  
  if(argc > 1){
    strcpy(safe_type,argv[1]);
  }


  u64 result = our_code_starts_here();
  print_res(result);

  return 0;
}
