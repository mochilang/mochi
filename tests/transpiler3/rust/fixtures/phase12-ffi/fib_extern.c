#include <stdint.h>
int64_t c_fib(int64_t n) {
  int64_t a = 0, b = 1;
  for (int64_t i = 0; i < n; i++) {
    int64_t t = a + b;
    a = b;
    b = t;
  }
  return a;
}
