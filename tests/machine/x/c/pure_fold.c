#include <stdio.h>
#include <stdlib.h>

int triple(int x) { return x * 3; }

int main() {
  printf("%d\n", triple(1 + 2));
  return 0;
}
