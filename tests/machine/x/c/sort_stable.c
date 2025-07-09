#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
  int len;
  char **data;
} list_string;
static list_string list_string_create(int len) {
  list_string l;
  l.len = len;
  l.data = (char **)malloc(sizeof(char *) * len);
  return l;
}
static void _print_list_string(list_string v) {
  for (int i = 0; i < v.len; i++) {
    if (i > 0)
      printf(" ");
    printf("%s", v.data[i]);
  }
}
typedef struct {
  int n;
  char *v;
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

int main() {
  list_itemsItem _t1 = list_itemsItem_create(3);
  _t1.data[0] = (itemsItem){.n = 1, .v = "a"};
  _t1.data[1] = (itemsItem){.n = 1, .v = "b"};
  _t1.data[2] = (itemsItem){.n = 2, .v = "c"};
  list_itemsItem items = _t1;
  list_string _t2 = list_string_create(items.len);
  int *_t5 = (int *)malloc(sizeof(int) * items.len);
  int _t3 = 0;
  for (int _t4 = 0; _t4 < items.len; _t4++) {
    itemsItem i = items.data[_t4];
    _t2.data[_t3] = i.v;
    _t5[_t3] = i.n;
    _t3++;
  }
  _t2.len = _t3;
  for (int i = 0; i < _t3 - 1; i++) {
    for (int j = i + 1; j < _t3; j++) {
      if (_t5[i] > _t5[j]) {
        int _t6 = _t5[i];
        _t5[i] = _t5[j];
        _t5[j] = _t6;
        char *_t7 = _t2.data[i];
        _t2.data[i] = _t2.data[j];
        _t2.data[j] = _t7;
      }
    }
  }
  list_string result = _t2;
  _print_list_string(result);
  printf("\n");
  return 0;
}
