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
static double _avg(list_int v) {
  if (v.len == 0)
    return 0;
  double sum = 0;
  for (int i = 0; i < v.len; i++)
    sum += v.data[i];
  return sum / v.len;
}
int main() {
  int _t1_data[] = {1, 2, 3};
  list_int _t1 = {3, _t1_data};
  printf("%.17g\n", _avg(_t1));
  return 0;
}
