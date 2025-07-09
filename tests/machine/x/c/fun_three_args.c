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
int sum3(int a, int b, int c) { return a + b + c; }

int main() {
  printf("%d\n", sum3(1, 2, 3));
  return 0;
}
