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
typedef struct {
  int len;
  list_int *data;
} list_list_int;
static list_list_int list_list_int_create(int len) {
  list_list_int l;
  l.len = len;
  l.data = (list_int *)malloc(sizeof(list_int) * len);
  return l;
}
int main() {
  list_list_int _t1 = list_list_int_create(2);
  list_int _t2 = list_int_create(2);
  _t2.data[0] = 1;
  _t2.data[1] = 2;
  _t1.data[0] = _t2;
  list_int _t3 = list_int_create(2);
  _t3.data[0] = 3;
  _t3.data[1] = 4;
  _t1.data[1] = _t3;
  __auto_type matrix = _t1;
  matrix.data[1].data[0] = 5;
  printf("%d\n", matrix.data[1].data[0]);
  return 0;
}
