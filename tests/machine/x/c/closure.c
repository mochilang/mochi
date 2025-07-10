#include <stdio.h>
#include <stdlib.h>

static int _lambda0_n;
int _lambda0(int x) { return x + _lambda0_n; }

int (*makeAdder(int n))(int) { return (_lambda0_n = n, _lambda0); }

int main() {
  __auto_type add10 = makeAdder(10);
  printf("%d\n", add10(7));
  return 0;
}
