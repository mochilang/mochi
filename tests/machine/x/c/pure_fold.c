// Generated by Mochi compiler v0.10.28 on 2006-01-02T15:04:05Z
#include <stdio.h>
#include <stdlib.h>

int triple(int x) { return x * 3; }

int _mochi_main() {
  printf("%d\n", triple(1 + 2));
  return 0;
}
int main() { return _mochi_main(); }
