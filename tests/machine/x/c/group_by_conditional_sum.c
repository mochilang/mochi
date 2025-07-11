#include <stdio.h>
#include <stdlib.h>

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
  char *cat;
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
  itemsItem _t1_data[] = {(itemsItem){.cat = "a", .val = 10, .flag = 1},
                          (itemsItem){.cat = "a", .val = 5, .flag = 0},
                          (itemsItem){.cat = "b", .val = 20, .flag = 1}};
  list_itemsItem _t1 = {3, _t1_data};
  list_itemsItem items = _t1;
  list_resultItem result = 0;
  printf("%d\n", result);
  return 0;
}
