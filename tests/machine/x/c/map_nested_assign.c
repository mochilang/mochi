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
  l.data = (outerItem *)malloc(sizeof(outerItem) * len);
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
  l.data = (dataItem *)malloc(sizeof(dataItem) * len);
  return l;
}

int main() {
  __auto_type data = (dataItem){.outer = (outerItem){.inner = 1}};
  data.outer.inner = 2;
  printf("%d\n", data.outer.inner);
  return 0;
}
