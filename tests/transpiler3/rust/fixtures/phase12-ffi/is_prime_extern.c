#include <stdint.h>
#include <stdbool.h>
bool c_is_prime(int64_t n) {
  if (n < 2) return false;
  for (int64_t i = 2; i * i <= n; i++) {
    if (n % i == 0) return false;
  }
  return true;
}
