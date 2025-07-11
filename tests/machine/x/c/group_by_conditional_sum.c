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
  char **tmp9 = (char **)malloc(sizeof(char *) * tmp6.len);
  int tmp8 = 0;
  for (int gi = 0; gi < tmp6.len; gi++) {
    _GroupString _gp = tmp6.data[gi];
    list_ItemsItem tmp10 = list_ItemsItem_create(_gp.items.len);
    for (int i11 = 0; i11 < _gp.items.len; i11++) {
      tmp10.data[i11] = tmp2.data[_gp.items.data[i11]];
    }
    tmp10.len = _gp.items.len;
    struct {
      char *key;
      list_ItemsItem items;
    } g = {_gp.key, tmp10};
    list_int tmp12 = list_int_create(g.items.len);
    int tmp13 = 0;
    for (int i14 = 0; i14 < g.items.len; i14++) {
      ItemsItem x = g.items.data[i14];
      tmp12.data[tmp13] = (x.flag ? x.val : 0);
      tmp13++;
    }
    tmp12.len = tmp13;
    list_int tmp15 = list_int_create(g.items.len);
    int tmp16 = 0;
    for (int i17 = 0; i17 < g.items.len; i17++) {
      ItemsItem x = g.items.data[i17];
      tmp15.data[tmp16] = x.val;
      tmp16++;
    }
    tmp15.len = tmp16;
    tmp7.data[tmp8] = (ResultItem){.cat = g.key,
                                   .share = ((double)_sum_int(tmp12)) /
                                            ((double)_sum_int(tmp15))};
    tmp9[tmp8] = g.key;
    tmp8++;
  }
  tmp7.len = tmp8;
  for (int i20 = 0; i20 < tmp8 - 1; i20++) {
    for (int i21 = i20 + 1; i21 < tmp8; i21++) {
      if (strcmp(tmp9[i20], tmp9[i21]) > 0) {
        char *tmp18 = tmp9[i20];
        tmp9[i20] = tmp9[i21];
        tmp9[i21] = tmp18;
        ResultItem tmp19 = tmp7.data[i20];
        tmp7.data[i20] = tmp7.data[i21];
        tmp7.data[i21] = tmp19;
      }
    }
  }
  list_ResultItem result = tmp7;
  for (int i22 = 0; i22 < result.len; i22++) {
    ResultItem it = result.data[i22];
    if (i22 > 0)
      printf(" ");
    printf("map[");
    printf("cat:");
    printf("%s", it.cat);
    printf(" ");
    printf("share:");
    printf("%.16g", it.share);
    printf("]");
  }
  printf("\n");
  return 0;
}
