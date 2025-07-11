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
  int _t1_data[] = {1, 2};
  list_int _t1 = {2, _t1_data};
  list_int data = _t1;
  list_int _t2 = list_int_create(data.len);
  int _t3 = 0;
  for (int _t4 = 0; _t4 < data.len; _t4++) {
    int x = data.data[_t4];
    if (!(x == 1)) {
      continue;
    }
    _t2.data[_t3] = x;
    _t3++;
  }
  _t2.len = _t3;
  int flag = _t2.len > 0;
  printf("%s\n", (flag) ? "true" : "false");
  return 0;
}
