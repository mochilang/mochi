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
  l.data = calloc(len, sizeof(mItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

int main() {
  mItem m = (mItem){.a = 1, .b = 2};
  for (int tmp1 = 0; tmp1 < m.len; tmp1++) {
    int k = m.data[tmp1];
    printf("%d\n", k);
  }
  return 0;
}
