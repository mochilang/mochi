#include <stdio.h>
#include <stdlib.h>

static int x = 3;
static int y = 4;

typedef struct {
  int a;
  int b;
} MItem;
typedef struct {
  int len;
  MItem *data;
} list_MItem;
static list_MItem list_MItem_create(int len) {
  list_MItem l;
  l.len = len;
  l.data = calloc(len, sizeof(MItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

int main() {
  mItem m = (MItem){.a = x, .b = y};
  printf("%d ", m.a);
  printf("%d\n", m.b);
  return 0;
}
