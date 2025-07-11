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
static int _sum_int(list_int v) {
  int sum = 0;
  for (int i = 0; i < v.len; i++)
    sum += v.data[i];
  return sum;
}
int main() {
  int _t1_data[] = {1, 2, 3};
  list_int _t1 = {3, _t1_data};
  printf("%.17g\n", _sum_int(_t1));
  return 0;
}
