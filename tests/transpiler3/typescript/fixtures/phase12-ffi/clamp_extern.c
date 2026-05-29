#include <stdint.h>
int64_t c_clamp(int64_t x, int64_t lo, int64_t hi) {
  if (x < lo) return lo;
  if (x > hi) return hi;
  return x;
}
