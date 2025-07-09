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
int inner(int y) { return (x + y); }

int outer(int x) { return inner(5); }

int main() {
  printf("%d\n", outer(3));
  return 0;
}
