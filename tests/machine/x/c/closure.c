// Generated by Mochi compiler v0.10.28 on 2006-01-02T15:04:05Z
#include <stdio.h>
#include <stdlib.h>

static int _lambda0_n;
int _lambda0(int x) { return x + _lambda0_n; }

int (*makeAdder(int n))(int) { return (_lambda0_n = n, _lambda0); }

int _mochi_main() {
  int (*add10)(int) = makeAdder(10);
  printf("%d\n", add10(7));
  return 0;
}
int main() { return _mochi_main(); }
