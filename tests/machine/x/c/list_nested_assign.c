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
  int _t2_data[] = {1, 2};
  list_int _t2 = {2, _t2_data};
  int _t3_data[] = {3, 4};
  list_int _t3 = {2, _t3_data};
  list_int _t1_data[] = {_t2, _t3};
  list_list_int _t1 = {2, _t1_data};
  list_list_int matrix = _t1;
  matrix.data[1].data[0] = 5;
  printf("%d\n", matrix.data[1].data[0]);
  return 0;
}
