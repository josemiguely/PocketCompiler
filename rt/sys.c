#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>
#include <stdlib.h>
#include <string.h>
#include <sys/resource.h>
#include <stdbool.h>

/* synonym to ease typing/reading */
typedef uint64_t u64;

/* configuration */
u64 STACK_SIZE = 0x800000;
u64 HEAP_SIZE = 16;
int USE_GC = 1;

/* externs */
extern u64 our_code_starts_here() asm("our_code_starts_here");
extern u64* try_gc(u64* alloc_ptr, u64 words_needed, u64* cur_frame, u64* cur_sp) asm("try_gc");
extern u64 our_code_starts_here(u64* heap) asm("our_code_starts_here");
extern void set_stack_bottom(u64* stack_bottom) asm("set_stack_bottom");

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

/* GC */
u64* HEAP_START;
u64* HEAP_END;
u64* HEAP_MID;
u64* TO_SPACE;
u64* FROM_SPACE;
u64* ALLOC_PTR = 0;
u64* SCAN_PTR = 0;
u64* STACK_BOTTOM = 0;

void set_stack_bottom(u64* stack_bottom) {
  STACK_BOTTOM = stack_bottom;
}

bool is_heap_ptr(u64 val){
  return (u64*)val < HEAP_END && (u64*)val >= HEAP_START;
}

void print_stack(u64* rbp, u64* rsp) {
  printf("|------- frame %p to %p  ------\n", rsp, rbp);
  for (u64* cur_word = rsp; cur_word < rbp; cur_word++) {
    u64 val = (u64)*cur_word;
    printf("| %p: %p", cur_word, (u64*)*cur_word);
    if (is_heap_ptr(val)) {
      if (is_tuple(val)){ printf(" (tuple)"); }
      else if (is_closure(val)){ printf(" (closure)"); }
    }
    printf("\n");
  }
  if (rbp < STACK_BOTTOM) {
    print_stack((u64*)*rbp, rbp + 2);
  }
  else {
    printf("|------- bottom %p  ------\n\n", STACK_BOTTOM);
  }
}

void print_heap(u64* heap_start, u64* heap_end){
  printf("| Heap from %p to %p\n", heap_start, heap_end);
  for (u64 i = 0; i <= (u64)(heap_end - heap_start); i++) {
    printf("|  %lld/%p: %p \n", i, (heap_start + i), (u64*)*(heap_start + i));
  }
}

void print_heaps(){
  printf("|\n|=======HEAP 1==========\n");
  print_heap(HEAP_START, HEAP_MID-1);
  printf("|=======HEAP 2==========\n");
  print_heap(HEAP_MID, HEAP_END);
  printf("|=================\n\n");
}


u64* collect(u64* cur_frame, u64* cur_sp) {
  /* TBD: see https://en.wikipedia.org/wiki/Cheney%27s_algorithm */
  // swap from-space to-space
  // init spaces
  // scan stack and copy roots
  // scan objects in the heap
  // clean old space
  return ALLOC_PTR;
}

/* trigger GC if enabled and needed, out-of-memory error if insufficient */
u64* try_gc(u64* alloc_ptr, u64 words_needed, u64* cur_frame, u64* cur_sp) {
  if (USE_GC==1 && alloc_ptr + words_needed > FROM_SPACE + HEAP_SIZE) {
    printf("| need memory: GC!\n");
    alloc_ptr = collect(cur_frame, cur_sp);
  }
  if (alloc_ptr + words_needed > FROM_SPACE + HEAP_SIZE) {
    printf("| Error: out of memory!\n\n");
    print_stack(cur_frame, cur_sp);
    print_heaps();
    exit(-1);
  }
  return alloc_ptr;
}

/* start */
int main(int argc, char** argv) {
  
  /* stack size config */
  char* stack_size_envvar = getenv("STACK_SIZE");
  if (stack_size_envvar) STACK_SIZE = strtoull(stack_size_envvar, NULL, 0);
  printf("| Setting stack size to %" PRId64 " .\n", STACK_SIZE);
  struct rlimit limit;
  getrlimit(RLIMIT_STACK, &limit);
  limit.rlim_cur = STACK_SIZE < limit.rlim_max ? STACK_SIZE : limit.rlim_max;
  int res = setrlimit(RLIMIT_STACK, &limit);
  if (res != 0) { printf("| Setting rlimit failed...\n") ; }
  
  /* GC config */
  char* use_gc_envvar = getenv("USE_GC");
  if (use_gc_envvar) USE_GC = strtoull(use_gc_envvar, NULL, 0);
  printf("| Use GC: %d\n", USE_GC);
  
  /* heap size config */
  char* heap_size_envvar = getenv("HEAP_SIZE");
  if (heap_size_envvar) HEAP_SIZE = strtoull(heap_size_envvar, NULL, 0);
  printf("| Heap size: %" PRId64 " .\n", HEAP_SIZE);


  char *SAFE = getenv("SAFE");

  //uint64_t* HEAP = calloc(1024, sizeof(uint64_t)); // Allocate 8KB of memory for now
 
  /* setting up two space heap for GC */
  u64* heap = (u64*)calloc((HEAP_SIZE * 2) + 15, sizeof(u64));
  HEAP_START = (u64*)(((u64)heap + 15) & ~0xF);
  /* TBD: initialize HEAP_MID, HEAP_END, FROM_SPACE, TO_SPACE */
  HEAP_MID = 0;   /* TBD */
  HEAP_END = 0;   /* TBD */
  FROM_SPACE = 0; /* TBD */
  TO_SPACE = 0;   /* TBD */
 
  if (SAFE){
     strcpy(safe_type,SAFE);
  }
  
  if(argc > 1){
    strcpy(safe_type,argv[1]);
  }

  
  u64 result = our_code_starts_here(heap);
  print_res(result);
  free(heap);
  return 0;
}
