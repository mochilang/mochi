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
  printf("%d\n", "a" in m);
  printf("%d\n", "c" in m);
  return 0;
}
