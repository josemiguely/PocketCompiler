#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>

typedef uint64_t u64;

extern u64 our_code_starts_here() asm("our_code_starts_here");

typedef uint64_t VAL;
const uint64_t BOOL_TAG   = 0x0000000000000001;
const VAL BOOL_TRUE =0xffffffffffffffff;
const VAL BOOL_FALSE = 0x7fffffffffffffff; // as chosen in compile.ml


VAL print(VAL val) {
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


int main(int argc, char** argv) {
  u64 result = our_code_starts_here();
  print(result);
  // printf("choriflay\n");
  // printf("%" PRId64 "\n", result);
  return 0;
}
