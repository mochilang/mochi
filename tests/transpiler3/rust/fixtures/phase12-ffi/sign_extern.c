#include <stdint.h>
int64_t c_sign(int64_t x) {
  if (x > 0) return 1;
  if (x < 0) return -1;
  return 0;
}
