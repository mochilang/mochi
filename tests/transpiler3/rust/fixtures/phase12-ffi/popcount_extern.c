#include <stdint.h>
int64_t c_popcount(int64_t x) {
  int64_t c = 0;
  while (x != 0) {
    c += x & 1;
    x = (int64_t)((uint64_t)x >> 1);
  }
  return c;
}
