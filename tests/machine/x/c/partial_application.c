#include <stdio.h>
#include <stdlib.h>

static int _partial0_arg0;
static int _partial0(int p0) { return add(_partial0_arg0, p0); }

int add(int a, int b) { return a + b; }

int main() {
  int (*add5)(int) = (_partial0_arg0 = 5, _partial0);
  printf("%d\n", add5(3));
  return 0;
}
