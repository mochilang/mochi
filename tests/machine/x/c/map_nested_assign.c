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
static void _print_list_outerItem(list_OuterItem v) {
  for (int i = 0; i < v.len; i++) {
    OuterItem s = v.data[i];
    printf("map[");
    printf("inner:");
    printf("%d", s.inner);
    printf("]");
    if (i < v.len - 1)
      printf(" ");
  }
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
static void _print_list_dataItem(list_DataItem v) {
  for (int i = 0; i < v.len; i++) {
    DataItem s = v.data[i];
    printf("map[");
    printf("outer:");
    printf("]");
    if (i < v.len - 1)
      printf(" ");
  }
}

int main() {
  DataItem data = (DataItem){.outer = (OuterItem){.inner = 1}};
  data.outer.inner = 2;
  printf("%d\n", data.outer.inner);
  return 0;
}
