#include <stdio.h>
#include <stdlib.h>

int _lambda0(int x) { return x * x; }

int main() {
  int (*square)(int) = _lambda0;
  printf("%.16g\n", square(6));
  return 0;
}
