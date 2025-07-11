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
static int _min_int(list_int v) {
  if (v.len == 0)
    return 0;
  int m = v.data[0];
  for (int i = 1; i < v.len; i++)
    if (v.data[i] < m)
      m = v.data[i];
  return m;
}
static int _max_int(list_int v) {
  if (v.len == 0)
    return 0;
  int m = v.data[0];
  for (int i = 1; i < v.len; i++)
    if (v.data[i] > m)
      m = v.data[i];
  return m;
}
int main() {
  int _t1_data[] = {3, 1, 4};
  list_int _t1 = {3, _t1_data};
  list_int nums = _t1;
  printf("%d\n", _min_int(nums));
  printf("%d\n", _max_int(nums));
  return 0;
}
