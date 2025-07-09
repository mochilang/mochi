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
static int contains_list_int(list_int v, int item) {
  for (int i = 0; i < v.len; i++)
    if (v.data[i] == item)
      return 1;
  return 0;
}
int main() {
  list_int _t1 = list_int_create(3);
  _t1.data[0] = 1;
  _t1.data[1] = 2;
  _t1.data[2] = 3;
  list_int nums = _t1;
  printf("%s\n", (contains_list_int(nums, 2)) ? "true" : "false");
  printf("%s\n", (contains_list_int(nums, 4)) ? "true" : "false");
  return 0;
}
