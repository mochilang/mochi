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
  l.data = calloc(len, sizeof(int));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}
typedef struct {
  int len;
  char **data;
} list_string;
static list_string list_string_create(int len) {
  list_string l;
  l.len = len;
  l.data = calloc(len, sizeof(char *));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
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
} ItemsItem;
typedef struct {
  int len;
  ItemsItem *data;
} list_ItemsItem;
static list_ItemsItem list_ItemsItem_create(int len) {
  list_ItemsItem l;
  l.len = len;
  l.data = calloc(len, sizeof(ItemsItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  char *cat;
  double total;
} GroupedItem;
typedef struct {
  int len;
  GroupedItem *data;
} list_GroupedItem;
static list_GroupedItem list_GroupedItem_create(int len) {
  list_GroupedItem l;
  l.len = len;
  l.data = calloc(len, sizeof(GroupedItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

int main() {
  ItemsItem tmp1_data[] = {
      (ItemsItem){.cat = "a", .val = 3}, (ItemsItem){.cat = "a", .val = 1},
      (ItemsItem){.cat = "b", .val = 5}, (ItemsItem){.cat = "b", .val = 2}};
  list_ItemsItem tmp1 = {4, tmp1_data};
  list_itemsItem items = tmp1;
  list_itemsItem tmp2 = list_itemsItem_create(items.len);
  list_string tmp3 = list_string_create(items.len);
  int tmp4 = 0;
  for (int i = 0; i < items.len; i++) {
    itemsItem i = items.data[i];
    tmp2.data[tmp4] = i;
    tmp3.data[tmp4] = i.cat;
    tmp4++;
  }
  tmp2.len = tmp4;
  tmp3.len = tmp4;
  list_group_string tmp5 = _group_by_string(tmp3);
  list_groupedItem tmp6 = list_groupedItem_create(tmp5.len);
  double *tmp8 = (double *)malloc(sizeof(double) * tmp5.len);
  int tmp7 = 0;
  for (int gi = 0; gi < tmp5.len; gi++) {
    _GroupString _gp = tmp5.data[gi];
    list_itemsItem tmp9 = list_itemsItem_create(_gp.items.len);
    for (int j = 0; j < _gp.items.len; j++) {
      tmp9.data[j] = tmp2.data[_gp.items.data[j]];
    }
    tmp9.len = _gp.items.len;
    struct {
      char *key;
      list_itemsItem items;
    } g = {_gp.key, tmp9};
    list_int tmp10 = list_int_create(g.items.len);
    int tmp11 = 0;
    for (int i = 0; i < g.items.len; i++) {
      itemsItem x = g.items.data[i];
      tmp10.data[tmp11] = x.val;
      tmp11++;
    }
    tmp10.len = tmp11;
    tmp6.data[tmp7] = (GroupedItem){.cat = g.key, .total = _sum_int(tmp10)};
    list_int tmp12 = list_int_create(g.items.len);
    int tmp13 = 0;
    for (int i = 0; i < g.items.len; i++) {
      itemsItem x = g.items.data[i];
      tmp12.data[tmp13] = x.val;
      tmp13++;
    }
    tmp12.len = tmp13;
    tmp8[tmp7] = (-_sum_int(tmp12));
    tmp7++;
  }
  tmp6.len = tmp7;
  for (int i = 0; i < tmp7 - 1; i++) {
    for (int j = i + 1; j < tmp7; j++) {
      if (tmp8[i] > tmp8[j]) {
        double tmp14 = tmp8[i];
        tmp8[i] = tmp8[j];
        tmp8[j] = tmp14;
        groupedItem tmp15 = tmp6.data[i];
        tmp6.data[i] = tmp6.data[j];
        tmp6.data[j] = tmp15;
      }
    }
  }
  list_GroupedItem grouped = tmp6;
  printf("%d\n", grouped);
  return 0;
}
