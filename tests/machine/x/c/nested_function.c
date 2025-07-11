#include <stdio.h>
#include <stdlib.h>

static int inner_x;
int inner(int y) { return inner_x + y; }

int outer(int x) { return (inner_x = x, inner(5)); }

int main() {
  printf("%d\n", outer(3));
  return 0;
}
