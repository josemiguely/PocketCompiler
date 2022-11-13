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
const VAL TUPLE_TAG = 0x1;
const VAL CLOSURE_TAG = 0x5;

char safe_type[2];

void tuple_print_res(VAL *,VAL);
VAL print_res(VAL);

//Recibre addres y cantidad de tuplaws
void tuple_print_res(VAL * addrcount,VAL count){
    // printf("count esss = %lu\n",count);
    if (count==0){
      printf("(tup");
    }
    else{
    printf("(tup ");
    }
    for (int i=1;i<count+1;i++){
    print_res(*(addrcount+i));
    if (i<count){
    printf(" ");
    }
     }
    printf(")");

}

VAL print_res(VAL val) {
 
  if ((val & BOOL_TAG) == 0) { // val is even ==> number
    printf("%ld", ((int64_t)(val)) / 2); // shift bits right to remove tag
  } else if (val == BOOL_TRUE) {
    printf("true");
  } else if (val == BOOL_FALSE) {
    printf("false");
  } 

    else if ((val & CLOSURE_TAG) == 5){

      VAL* addrcount= (VAL*) (val-(VAL)5); //Untaggeamos la clausura
      VAL count= *(addrcount);
      // printf("count = %#018x\n",count);
      //printf("primer valor %#018x\n",*(addrcount+1));
      
      //printf("count = %ld", ((int64_t)(count))); // 
      printf("<clos:%ld>",(int64_t)(count));
      
      
      
    }

  
    else if ((val & TUPLE_TAG) == 1){
      VAL* addrcount= (VAL*) (val-(VAL)1);
      VAL count= *addrcount;
      
      
      tuple_print_res(addrcount,count); // Se le entrega addr y cantidad de tuplas
      
      
    }
  else {
    printf("Unknown value: %#018x", (unsigned int)val); // print unknown val in hex
  }
  return val;
}
// operation type

VAL print(VAL val) {
  printf("> ");
  print_res(val);
  printf("\n");
  return val;
}

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

  if(*safe_type == '2' && b2 !=0 && a2!=0 ){

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


    if (b2 < 0){
      b2 = b2*-1;
    }

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
const int ERR_NOT_TUPLE=3;
const int ERR_NOT_CLOSURE=4;

void error(int errCode, VAL val){
 
   if (errCode == ERR_NOT_NUMBER){
    printf("Type error: Expected integer, but got ");
    print_res(val);
  
    
  }

  else if (errCode == ERR_NOT_BOOLEAN){
    printf("Type error: Expected boolean, but got ");
    print_res(val);
  
  }

  else if (errCode == ERR_NOT_TUPLE){
    printf("Type error: Expected tuple but got ");
    print_res(val);
  }

  else if (errCode == ERR_NOT_CLOSURE){
    printf("Type error: Expected closure but got ");
    print_res(val);
  }


  else{
    printf("Error code not recognized, mayor bug in code");
  }
  
  exit(errCode);
}

void tuple_index_error (VAL* tuple,int index){
  printf("Index out of bounds: Tried to access index %i of ",index);
  VAL count = *(tuple);
  tuple_print_res(tuple,count);
  exit(-1);
}

void closure_arity_mismatch (int expected_arity,int arity){

  printf("Arity mismatch: closure expected %i arguments but got %i",expected_arity,arity);

  exit(-1);
}


int main(int argc, char** argv) {
  char *SAFE = getenv("SAFE");

  uint64_t* HEAP = calloc(1024, sizeof(uint64_t)); // Allocate 8KB of memory for now
  
 
  if (SAFE){
     strcpy(safe_type,SAFE);
  }
  
  if(argc > 1){
    strcpy(safe_type,argv[1]);
  }

  
  u64 result = our_code_starts_here(HEAP);
  print_res(result);
  free(HEAP);
  return 0;
}
