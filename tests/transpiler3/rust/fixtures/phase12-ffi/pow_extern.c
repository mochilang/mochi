#include <stdint.h>
int64_t c_pow(int64_t base, int64_t exp) {
  int64_t r = 1;
  for (int64_t i = 0; i < exp; i++) r *= base;
  return r;
}
