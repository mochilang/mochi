#include <stdio.h>
#include <stdlib.h>

int _lambda0(int x) { return x + n; }

int (*)(int) makeAdder(int n) { return _lambda0; }

int main() {
  int (*add10)(int) = makeAdder(10);
  printf("%d\n", add10(7));
  return 0;
}
