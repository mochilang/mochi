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
int triple(int x) { return x * 3; }

int main() {
  printf("%d\n", triple(1 + 2));
  return 0;
}
