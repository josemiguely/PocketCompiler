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
void print_heaps ();
typedef uint64_t VAL;
const uint64_t BOOL_TAG   = 0x0000000000000001;
const VAL BOOL_TRUE =       0xffffffffffffffff;
const VAL BOOL_FALSE =      0x7fffffffffffffff; // as chosen in compile.ml
const VAL TUPLE_TAG = 0x1;
const VAL CLOSURE_TAG = 0x5;
const VAL FORWARDED_TAG = 0x7;

char safe_type[2];

void tuple_print_res(VAL *,VAL);
VAL print_res(VAL);

//Recibre addres y cantidad de tuplaws
void tuple_print_res(VAL * addrcount,VAL count){
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

      printf("<clos:%ld>",(int64_t)(count));
      
      
      
    }

  
    else if ((val & TUPLE_TAG) == 1){
      
      VAL* addrcount= (VAL*) (val-(VAL)1);//Le sacamos el tag y conseguimos el puntero
      
      VAL count= *addrcount; // En la primera posición del puntero tenemos la cantidad de elementos en la tupla
      
      
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
    print_heaps();
    printf("VALL = %#018x\n",(unsigned int)val);
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

void set_stack_bottom (u64* stack_bottom) {
  STACK_BOTTOM = stack_bottom;
}

bool is_heap_ptr (u64 val){
  return (u64*)val < HEAP_END && (u64*)val >= HEAP_START;
}


bool is_tuple (u64 val){
  return (val & TUPLE_TAG) == 1;
}

bool is_closure (u64 val){
  return (val & CLOSURE_TAG) == 5;
}

bool is_forwarded(VAL val){
  return (val & FORWARDED_TAG) == 7;
}


void print_stack (u64* rbp, u64* rsp) {
  printf("|------- frame %p to %p  ------\n", rsp, rbp);
  for (u64* cur_word = rsp; cur_word < rbp; cur_word++) {
    u64 val = (u64)*cur_word;
    printf("| %p: %p", cur_word, (u64*)*cur_word);
    if (is_heap_ptr(val)) {
      
      if (is_closure(val)){ printf(" (closure)"); }
      else if (is_tuple(val)){ printf(" (tuple)"); }
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

void print_heap (u64* heap_start, u64* heap_end){
  printf("| Heap from %p to %p\n", heap_start, heap_end);
  for (u64 i = 0; i <= (u64)(heap_end - heap_start); i++) {
    printf("|  %lu/%p: %p \n", i, (heap_start + i), (u64*)*(heap_start + i));
  }
}

void print_heaps (){
  printf("|\n|=======HEAP 1==========\n");
  print_heap(HEAP_START, HEAP_MID-1);
  printf("|=======HEAP 2==========\n");
  print_heap(HEAP_MID, HEAP_END);
  printf("|=================\n\n");
}

VAL forwarding_adress (VAL val){
  
  if(is_closure(val)){
    return val-5+7;
  }

  else if (is_tuple(val)){
    return val-1+7;
  }

  else { // Es forwarded
    return val;
  }

}


VAL size(VAL val){

  if (is_closure((VAL) val)){
    
    VAL* pointer= (VAL*) (val-CLOSURE_TAG); //Untaggeamos la clausura
    VAL size_of_free_vars= *(pointer+2);

    return 3+size_of_free_vars;
  }

  if(is_tuple((VAL) val)){

    VAL* pointer= (VAL*) (val-TUPLE_TAG); //Untaggeamos la tupla
    VAL size_of_tuple= *(pointer);

    return 1+size_of_tuple;
    
  }

  return 1; // Si es un booleano o un numero su tamaño es 1 , este caso nunca deberia pasar.

}


u64 copy(VAL* o){
  
    if (!is_forwarded((u64) o)){
    
    //Si es clausura entonces encontramos raiz que referencia al heap, entonces debemos copiarla
    if (is_closure((VAL) o)){
         
        VAL* closure= (VAL *)( ((VAL) o)-5); // Le sacamos el tag
        
        if (is_forwarded(*closure)){
          return ((VAL)*closure - FORWARDED_TAG);
        }

        u64* oprim= ALLOC_PTR;
        u64* alloc_temp= ALLOC_PTR;
        ALLOC_PTR +=  size((VAL)o);
        
        for (int i=0;i<size((VAL)o);i++){
          *(alloc_temp+i) = *(closure + i);
        }
        
        *closure=(u64)oprim+FORWARDED_TAG;
        return (u64)oprim;
    }
  
    //Si es tupla entonces encontramos raiz que referencia al heap, entonces debemos copiarla
      else{
    
        VAL* tuple=(VAL *)(((VAL )o)-TUPLE_TAG); // Le sacamos el tag
    
        if (is_forwarded(*tuple)){
          return ((VAL)*tuple - FORWARDED_TAG);
        }
      
        u64* oprim= ALLOC_PTR;
        u64* alloc_temp= ALLOC_PTR;
        ALLOC_PTR += size((VAL)o);
  
        //copy from alloc to alloc_temp
        for (int i=0;i<size((VAL)o);i++){
          *alloc_temp = *(tuple + i);
          alloc_temp++;
        }
        
        *tuple=(u64)oprim+FORWARDED_TAG;
        
        return (u64)oprim;

      }
    }
    else{ // Es un forwarded, por lo que debemos devolver la dirección sin el tag de forwarded
      //Este caso nunca deberia pasar
      return (u64)o - FORWARDED_TAG;
    }
    
}


void scan_objects(){
  
  while (SCAN_PTR<ALLOC_PTR){
        VAL o=*SCAN_PTR;  
        
        if((is_closure(o) || is_tuple(o)) && is_heap_ptr(o)){
        u64 tag = o & 7LL;   
        *SCAN_PTR=(VAL)copy((VAL *)o)+tag;
        }

        SCAN_PTR+=1;
        
  }

}

void scan_roots(u64* cur_frame, u64* cur_sp){
 
  while(cur_sp < STACK_BOTTOM){
  
  for (u64* cur_word = cur_sp; cur_word < cur_frame; cur_word++) {
    u64 root = (u64)*cur_word;
    //*cur_word = direccion retorno copy;
    if((is_closure(root) || is_tuple(root)) && is_heap_ptr(root)){
      u64 tag = root & 7LL; 
      root=(u64)copy((VAL *)root)+tag;
      *cur_word = root;
    }


    }
    cur_sp = cur_frame + 2;
    cur_frame = (u64 *)*cur_frame;
  }
}

u64* collect(u64* cur_frame, u64* cur_sp) {
  
  /* TBD: see https://en.wikipedia.org/wiki/Cheney%27s_algorithm */
  // swap from-space to-space
  u64* temp = FROM_SPACE;
  FROM_SPACE = TO_SPACE;
  TO_SPACE = temp;
  // init spaces
  ALLOC_PTR = TO_SPACE;  // El que me devuelve la nueva posicion donde alocar
  SCAN_PTR = TO_SPACE; // solo para la etapa del heap en el to_space

  
  // scan stack and copy roots
  scan_roots(cur_frame,cur_sp);

  // scan objects in the heap
  scan_objects();

  // clean old space
  for (int i=0;i<HEAP_SIZE;i++){
    FROM_SPACE[i] = 0;
  }

  return ALLOC_PTR;
}

/* trigger GC if enabled and needed, out-of-memory error if insufficient */
u64* try_gc(u64* alloc_ptr, u64 words_needed, u64* cur_frame, u64* cur_sp) {
  
  if (USE_GC==1 && alloc_ptr + words_needed > TO_SPACE + HEAP_SIZE) {

    printf("| need memory: GC!\n");
    alloc_ptr = collect(cur_frame, cur_sp);
    
  }
   
  if (alloc_ptr + words_needed > TO_SPACE + HEAP_SIZE) {
    printf("| Error: out of memory!\n\n");

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
 
  /* setting up two space heap for GC */
  u64* heap = (u64*)calloc((HEAP_SIZE * 2) + 7, sizeof(u64));
  HEAP_START = (u64*)(((u64)heap + 7) & ~0x7);

  HEAP_MID = HEAP_START + HEAP_SIZE;
  HEAP_END = HEAP_START + (HEAP_SIZE*2 - 1);
  TO_SPACE = HEAP_START;
  FROM_SPACE = HEAP_MID;
 
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
