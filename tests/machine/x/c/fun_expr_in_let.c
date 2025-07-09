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
int _lambda0(int x) { return (x * x); }

int main() {
  int (*square)(int) = _lambda0;
  printf("%d\n", square(6));
  return 0;
}
