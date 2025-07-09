#include <stdio.h>
#include <stdlib.h>

int inner(int y) { return x + y; }

int outer(int x) { return inner(5); }

int main() {
  printf("%d\n", outer(3));
  return 0;
}
