#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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
  int len;
  char **data;
} list_string;
static list_string list_string_create(int len) {
  list_string l;
  l.len = len;
  l.data = (char **)malloc(sizeof(char *) * len);
  return l;
}
static int _sum_int(list_int v) {
  int sum = 0;
  for (int i = 0; i < v.len; i++)
    sum += v.data[i];
  return sum;
}
typedef struct {
  char *key;
  list_int items;
} _GroupString;
typedef struct {
  int len;
  int cap;
  _GroupString *data;
} list_group_string;
static list_group_string _group_by_string(list_string src) {
  list_group_string res;
  res.len = 0;
  res.cap = 0;
  res.data = NULL;
  for (int i = 0; i < src.len; i++) {
    char *key = src.data[i];
    int idx = -1;
    for (int j = 0; j < res.len; j++)
      if (strcmp(res.data[j].key, key) == 0) {
        idx = j;
        break;
      }
    if (idx == -1) {
      if (res.len >= res.cap) {
        res.cap = res.cap ? res.cap * 2 : 4;
        res.data =
            (_GroupString *)realloc(res.data, sizeof(_GroupString) * res.cap);
      }
      res.data[res.len].key = key;
      res.data[res.len].items = list_int_create(0);
      idx = res.len++;
    }
    _GroupString *g = &res.data[idx];
    g->items.data =
        (int *)realloc(g->items.data, sizeof(int) * (g->items.len + 1));
    g->items.data[g->items.len++] = i;
  }
  return res;
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
  list_itemsItem _t2 = list_itemsItem_create(items.len);
  list_string _t3 = list_string_create(items.len);
  int _t4 = 0;
  for (int i = 0; i < items.len; i++) {
    itemsItem i = items.data[i];
    _t2.data[_t4] = i;
    _t3.data[_t4] = i.cat;
    _t4++;
  }
  _t2.len = _t4;
  _t3.len = _t4;
  list_group_string _t5 = _group_by_string(_t3);
  list_groupedItem _t6 = list_groupedItem_create(_t5.len);
  double *_t8 = (double *)malloc(sizeof(double) * _t5.len);
  int _t7 = 0;
  for (int gi = 0; gi < _t5.len; gi++) {
    _GroupString _gp = _t5.data[gi];
    list_itemsItem _t9 = list_itemsItem_create(_gp.items.len);
    for (int j = 0; j < _gp.items.len; j++) {
      _t9.data[j] = _t2.data[_gp.items.data[j]];
    }
    _t9.len = _gp.items.len;
    struct {
      char *key;
      list_itemsItem items;
    } g = {_gp.key, _t9};
    list_int _t10 = list_int_create(g.items.len);
    int _t11 = 0;
    for (int i = 0; i < g.items.len; i++) {
      itemsItem x = g.items.data[i];
      _t10.data[_t11] = x.val;
      _t11++;
    }
    _t10.len = _t11;
    _t6.data[_t7] = (groupedItem){.cat = g.key, .total = _sum_int(_t10)};
    list_int _t12 = list_int_create(g.items.len);
    int _t13 = 0;
    for (int i = 0; i < g.items.len; i++) {
      itemsItem x = g.items.data[i];
      _t12.data[_t13] = x.val;
      _t13++;
    }
    _t12.len = _t13;
    _t8[_t7] = (-_sum_int(_t12));
    _t7++;
  }
  _t6.len = _t7;
  for (int i = 0; i < _t7 - 1; i++) {
    for (int j = i + 1; j < _t7; j++) {
      if (_t8[i] > _t8[j]) {
        double _t14 = _t8[i];
        _t8[i] = _t8[j];
        _t8[j] = _t14;
        groupedItem _t15 = _t6.data[i];
        _t6.data[i] = _t6.data[j];
        _t6.data[j] = _t15;
      }
    }
  }
  list_groupedItem grouped = _t6;
  printf("%d\n", grouped);
  return 0;
}
