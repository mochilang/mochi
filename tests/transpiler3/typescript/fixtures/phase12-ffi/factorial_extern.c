#include <stdint.h>
int64_t c_factorial(int64_t n) {
  int64_t r = 1;
  for (int64_t i = 2; i <= n; i++) r *= i;
  return r;
}
