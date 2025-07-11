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
  int flag;
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
  double share;
} ResultItem;
typedef struct {
  int len;
  ResultItem *data;
} list_ResultItem;
static list_ResultItem list_ResultItem_create(int len) {
  list_ResultItem l;
  l.len = len;
  l.data = calloc(len, sizeof(ResultItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

int main() {
  ItemsItem tmp1_data[] = {(ItemsItem){.cat = "a", .val = 10, .flag = 1},
                           (ItemsItem){.cat = "a", .val = 5, .flag = 0},
                           (ItemsItem){.cat = "b", .val = 20, .flag = 1}};
  list_ItemsItem tmp1 = {3, tmp1_data};
  list_ItemsItem items = tmp1;
  list_ItemsItem tmp2 = list_ItemsItem_create(items.len);
  list_string tmp3 = list_string_create(items.len);
  int tmp4 = 0;
  for (int i5 = 0; i5 < items.len; i5++) {
    ItemsItem i = items.data[i5];
    tmp2.data[tmp4] = i;
    tmp3.data[tmp4] = i.cat;
    tmp4++;
  }
  tmp2.len = tmp4;
  tmp3.len = tmp4;
  list_group_string tmp6 = _group_by_string(tmp3);
  list_ResultItem tmp7 = list_ResultItem_create(tmp6.len);
  int *tmp9 = (int *)malloc(sizeof(int) * tmp6.len);
  int tmp8 = 0;
  for (int gi10 = 0; gi10 < tmp6.len; gi10++) {
    _GroupString _gp = tmp6.data[gi10];
    list_ItemsItem tmp11 = list_ItemsItem_create(_gp.items.len);
    for (int j12 = 0; j12 < _gp.items.len; j12++) {
      tmp11.data[j12] = tmp2.data[_gp.items.data[j12]];
    }
    tmp11.len = _gp.items.len;
    struct {
      char *key;
      list_ItemsItem items;
    } g = {_gp.key, tmp11};
    list_int tmp13 = list_int_create(g.items.len);
    int tmp14 = 0;
    for (int i15 = 0; i15 < g.items.len; i15++) {
      ItemsItem x = g.items.data[i15];
      tmp13.data[tmp14] = (x.flag ? x.val : 0);
      tmp14++;
    }
    tmp13.len = tmp14;
    list_int tmp16 = list_int_create(g.items.len);
    int tmp17 = 0;
    for (int i18 = 0; i18 < g.items.len; i18++) {
      ItemsItem x = g.items.data[i18];
      tmp16.data[tmp17] = x.val;
      tmp17++;
    }
    tmp16.len = tmp17;
    tmp7.data[tmp8] =
        (ResultItem){.cat = g.key, .share = _sum_int(tmp13) / _sum_int(tmp16)};
    tmp9[tmp8] = g.key;
    tmp8++;
  }
  tmp7.len = tmp8;
  for (int i = 0; i < tmp8 - 1; i++) {
    for (int j = i + 1; j < tmp8; j++) {
      if (tmp9[i] > tmp9[j]) {
        int tmp19 = tmp9[i];
        tmp9[i] = tmp9[j];
        tmp9[j] = tmp19;
        ResultItem tmp20 = tmp7.data[i];
        tmp7.data[i] = tmp7.data[j];
        tmp7.data[j] = tmp20;
      }
    }
  }
  list_ResultItem result = tmp7;
  printf("<list>\n");
  return 0;
}
