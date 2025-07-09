#include <stdio.h>
#include <stdlib.h>

typedef struct {
  int len;
  int cap;
  int *data;
} list_int;
static list_int list_int_create(int len) {
  list_int l;
  l.len = len;
  l.cap = len;
  l.data = len ? (int *)malloc(sizeof(int) * len) : NULL;
  return l;
}
static void list_int_free(list_int *l) {
  free(l->data);
  l->data = NULL;
  l->len = 0;
  l->cap = 0;
}
typedef struct {
  char *cat;
  int val;
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
  double total;
} groupedItem;
typedef struct {
  int len;
  groupedItem *data;
} list_groupedItem;
static list_groupedItem list_groupedItem_create(int len) {
  list_groupedItem l;
  l.len = len;
  l.data = (groupedItem *)malloc(sizeof(groupedItem) * len);
  return l;
}

int main() {
  list_itemsItem _t1 = list_itemsItem_create(4);
  _t1.data[0] = (itemsItem){.cat = "a", .val = 3};
  _t1.data[1] = (itemsItem){.cat = "a", .val = 1};
  _t1.data[2] = (itemsItem){.cat = "b", .val = 5};
  _t1.data[3] = (itemsItem){.cat = "b", .val = 2};
  list_itemsItem items = _t1;
  list_groupedItem grouped = 0;
  printf("%d\n", grouped);
  return 0;
}
