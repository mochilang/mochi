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
static char *s = "hello";

typedef struct {
  int a;
} mItem;
typedef struct {
  int len;
  mItem *data;
} list_mItem;
static list_mItem list_mItem_create(int len) {
  list_mItem l;
  l.len = len;
  l.data = (mItem *)malloc(sizeof(mItem) * len);
  return l;
}

int main() {
  list_int _t1 = list_int_create(3);
  _t1.data[0] = 1;
  _t1.data[1] = 2;
  _t1.data[2] = 3;
  list_int xs = _t1;
  list_int _t2 = list_int_create(xs.len);
  int _t3 = 0;
  for (int _t4 = 0; _t4 < xs.len; _t4++) {
    int x = xs.data[_t4];
    if (!(x % 2 == 1)) {
      continue;
    }
    _t2.data[_t3] = x;
    _t3++;
  }
  _t2.len = _t3;
  list_int ys = _t2;
  printf("%s\n", (contains_list_int(ys, 1)) ? "true" : "false");
  printf("%s\n", (contains_list_int(ys, 2)) ? "true" : "false");
  mItem m = (mItem){.a = 1};
  printf("%d\n", "a" in m);
  printf("%d\n", "b" in m);
  printf("%d\n", "ell" in s);
  printf("%d\n", "foo" in s);
  return 0;
}
