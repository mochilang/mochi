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
int inc(int x) { return (x + k); }

int main() {
  int k = 2;
  printf("%d\n", inc(3));
  return 0;
}
