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
int main() {
  list_int _t1 = list_int_create(3);
  _t1.data[0] = 10;
  _t1.data[1] = 20;
  _t1.data[2] = 30;
  __auto_type xs = _t1;
  printf("%d\n", xs.data[1]);
  return 0;
}
