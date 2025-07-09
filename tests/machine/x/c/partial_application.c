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
int add(int a, int b) { return (a + b); }

int main() {
  int (*add5)(int) = add(5);
  printf("%d\n", add5(3));
  return 0;
}
