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
  int _t1_data[] = {10, 20, 30};
  list_int _t1 = {3, _t1_data};
  list_int xs = _t1;
  printf("%d\n", xs.data[1]);
  return 0;
}
