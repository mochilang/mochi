#include <stdio.h>
#include <stdlib.h>

typedef struct {
  int a;
  int b;
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
  mItem m = (mItem){.a = 1, .b = 2};
  for (int _t1 = 0; _t1 < m.len; _t1++) {
    int k = m.data[_t1];
    printf("%d\n", k);
  }
  return 0;
}
