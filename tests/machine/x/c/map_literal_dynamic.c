#include <stdio.h>
#include <stdlib.h>

static int x = 3;
static int y = 4;

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
  mItem m = (mItem){.a = x, .b = y};
  printf("%d ", m.a);
  printf("%d\n", m.b);
  return 0;
}
