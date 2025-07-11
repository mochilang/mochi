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
  int _t1_data[] = {1, 2, 3};
  list_int _t1 = {3, _t1_data};
  for (int _t2 = 0; _t2 < _t1.len; _t2++) {
    int n = _t1.data[_t2];
    printf("%d\n", n);
  }
  return 0;
}
