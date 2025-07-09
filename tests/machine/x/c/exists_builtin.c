#include <stdio.h>
#include <stdlib.h>

typedef struct {
  int len;
  int cap;
  int *data;
} list_int;
static list_int list_int_create(int len) {
  list_int l;
  l.len = len;
  l.cap = len;
  l.data = len ? (int *)malloc(sizeof(int) * len) : NULL;
  return l;
}
static void list_int_free(list_int *l) {
  free(l->data);
  l->data = NULL;
  l->len = 0;
  l->cap = 0;
}
int main() {
  list_int _t1 = list_int_create(2);
  _t1.data[0] = 1;
  _t1.data[1] = 2;
  list_int data = _t1;
  list_int _t2 = list_int_create(data.len);
  int _t3 = 0;
  for (int _t4 = 0; _t4 < data.len; _t4++) {
    int x = data.data[_t4];
    if (!((x == 1))) {
      continue;
    }
    _t2.data[_t3] = x;
    _t3++;
  }
  _t2.len = _t3;
  int flag = exists(_t2);
  printf("%s\n", (flag) ? "true" : "false");
  return 0;
}
