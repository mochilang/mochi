#include <stdio.h>
#include <stdlib.h>

typedef struct {
  int len;
  int *data;
} list_int;
static list_int list_int_create(int len) {
  list_int l;
  l.len = len;
  l.data = (int *)malloc(sizeof(int) * len);
  return l;
}
int _lambda0(int x) { return x + n; }

int (*)(int) makeAdder(int n) { return _lambda0; }

int main() {
  int (*add10)(int) = makeAdder(10);
  printf("%d\n", add10(7));
  return 0;
}
