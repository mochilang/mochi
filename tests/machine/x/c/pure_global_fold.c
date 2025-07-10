#include <stdio.h>
#include <stdlib.h>

static int k = 2;

int inc(int x) { return x + k; }

int main() {
  printf("%d\n", inc(3));
  return 0;
}
