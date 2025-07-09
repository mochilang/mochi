#include <stdio.h>
#include <stdlib.h>

typedef struct {
  int len;
  int *data;
} list_int;
static list_int list_int_create(int len) {
  list_int l;
  l.len = len;
  l.data = (int *)malloc(sizeof(int) * len);
  return l;
}
typedef struct {
  char *cat;
  int val;
  int flag;
} itemsItem;
typedef struct {
  int len;
  itemsItem *data;
} list_itemsItem;
static list_itemsItem list_itemsItem_create(int len) {
  list_itemsItem l;
  l.len = len;
  l.data = (itemsItem *)malloc(sizeof(itemsItem) * len);
  return l;
}

typedef struct {
  int cat;
  double share;
} resultItem;
typedef struct {
  int len;
  resultItem *data;
} list_resultItem;
static list_resultItem list_resultItem_create(int len) {
  list_resultItem l;
  l.len = len;
  l.data = (resultItem *)malloc(sizeof(resultItem) * len);
  return l;
}

int main() {
  list_itemsItem _t1 = list_itemsItem_create(3);
  _t1.data[0] = (itemsItem){.cat = "a", .val = 10, .flag = 1};
  _t1.data[1] = (itemsItem){.cat = "a", .val = 5, .flag = 0};
  _t1.data[2] = (itemsItem){.cat = "b", .val = 20, .flag = 1};
  list_itemsItem items = _t1;
  list_resultItem result = 0;
  printf("%d\n", result);
  return 0;
}
