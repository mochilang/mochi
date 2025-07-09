#include <stdio.h>
#include <stdlib.h>

int add(int a, int b) { return a + b; }

int main() {
  int (*add5)(int) = add(5);
  printf("%d\n", add5(3));
  return 0;
}
