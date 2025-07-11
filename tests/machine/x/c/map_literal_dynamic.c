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
  l.data = calloc(len, sizeof(mItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

int main() {
  mItem m = (mItem){.a = x, .b = y};
  printf("%.16g ", m.a);
  printf("%.16g\n", m.b);
  return 0;
}
