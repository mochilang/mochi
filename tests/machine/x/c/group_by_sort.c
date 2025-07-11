#include <stdio.h>
#include <stdlib.h>

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
  char *cat;
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
  itemsItem _t1_data[] = {
      (itemsItem){.cat = "a", .val = 3}, (itemsItem){.cat = "a", .val = 1},
      (itemsItem){.cat = "b", .val = 5}, (itemsItem){.cat = "b", .val = 2}};
  list_itemsItem _t1 = {4, _t1_data};
  list_itemsItem items = _t1;
  list_groupedItem grouped = 0;
  printf("%d\n", grouped);
  return 0;
}
