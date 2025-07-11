#include <stdio.h>
#include <stdlib.h>

typedef struct {
  int inner;
} OuterItem;
typedef struct {
  int len;
  OuterItem *data;
} list_OuterItem;
static list_OuterItem list_OuterItem_create(int len) {
  list_OuterItem l;
  l.len = len;
  l.data = calloc(len, sizeof(OuterItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  OuterItem outer;
} DataItem;
typedef struct {
  int len;
  DataItem *data;
} list_DataItem;
static list_DataItem list_DataItem_create(int len) {
  list_DataItem l;
  l.len = len;
  l.data = calloc(len, sizeof(DataItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

int main() {
  DataItem data = (DataItem){.outer = (OuterItem){.inner = 1}};
  data.outer.inner = 2;
  printf("%d\n", data.outer.inner);
  return 0;
}
