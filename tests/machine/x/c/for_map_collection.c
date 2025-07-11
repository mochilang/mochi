#include <stdio.h>
#include <stdlib.h>

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
  mItem m = (MItem){.a = 1, .b = 2};
  for (int tmp1 = 0; tmp1 < m.len; tmp1++) {
    int k = m.data[tmp1];
    printf("%d\n", k);
  }
  return 0;
}
