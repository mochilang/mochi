#include <stdio.h>
#include <stdlib.h>

typedef struct {
  int inner;
} outerItem;
typedef struct {
  int len;
  outerItem *data;
} list_outerItem;
static list_outerItem list_outerItem_create(int len) {
  list_outerItem l;
  l.len = len;
  l.data = calloc(len, sizeof(outerItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  outerItem outer;
} dataItem;
typedef struct {
  int len;
  dataItem *data;
} list_dataItem;
static list_dataItem list_dataItem_create(int len) {
  list_dataItem l;
  l.len = len;
  l.data = calloc(len, sizeof(dataItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

int main() {
  dataItem data = (dataItem){.outer = (outerItem){.inner = 1}};
  data.outer.inner = 2;
  printf("%.16g\n", data.outer.inner);
  return 0;
}
