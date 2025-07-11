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
  MItem m = (MItem){.a = 1, .b = 2};
  printf("%d\n", "a" in m);
  printf("%d\n", "c" in m);
  return 0;
}
