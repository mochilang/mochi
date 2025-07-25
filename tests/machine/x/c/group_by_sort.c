// Generated by Mochi compiler v0.10.28 on 2006-01-02T15:04:05Z
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
} item_t;
typedef struct {
  int len;
  item_t *data;
} item_list_t;
item_list_t create_item_list(int len) {
  item_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(item_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct {
  char *cat;
  double total;
} grouped_item_t;
typedef struct {
  int len;
  grouped_item_t *data;
} grouped_item_list_t;
grouped_item_list_t create_grouped_item_list(int len) {
  grouped_item_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(grouped_item_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

int _mochi_main() {
  item_t items[] = {
      (item_t){.cat = "a", .val = 3}, (item_t){.cat = "a", .val = 1},
      (item_t){.cat = "b", .val = 5}, (item_t){.cat = "b", .val = 2}};
  int items_len = sizeof(items) / sizeof(items[0]);
  item_list_t tmp1 = create_item_list(items_len);
  list_string tmp2 = list_string_create(items_len);
  int tmp3 = 0;
  for (int i4 = 0; i4 < items_len; i4++) {
    item_t i = items[i4];
    tmp1.data[tmp3] = i;
    tmp2.data[tmp3] = i.cat;
    tmp3++;
  }
  tmp1.len = tmp3;
  tmp2.len = tmp3;
  list_group_string tmp5 = _group_by_string(tmp2);
  grouped_item_list_t tmp6 = create_grouped_item_list(tmp5.len);
  double *tmp8 = (double *)malloc(sizeof(double) * tmp5.len);
  int tmp7 = 0;
  for (int gi = 0; gi < tmp5.len; gi++) {
    _GroupString _gp = tmp5.data[gi];
    item_list_t tmp9 = create_item_list(_gp.items.len);
    for (int i10 = 0; i10 < _gp.items.len; i10++) {
      tmp9.data[i10] = tmp1.data[_gp.items.data[i10]];
    }
    tmp9.len = _gp.items.len;
    struct {
      char *key;
      item_list_t items;
    } g = {_gp.key, tmp9};
    list_int tmp11 = list_int_create(g.items.len);
    int tmp12 = 0;
    for (int i13 = 0; i13 < g.items.len; i13++) {
      item_t x = g.items.data[i13];
      tmp11.data[tmp12] = x.val;
      tmp12++;
    }
    tmp11.len = tmp12;
    tmp6.data[tmp7] = (grouped_item_t){.cat = g.key, .total = _sum_int(tmp11)};
    list_int tmp14 = list_int_create(g.items.len);
    int tmp15 = 0;
    for (int i16 = 0; i16 < g.items.len; i16++) {
      item_t x = g.items.data[i16];
      tmp14.data[tmp15] = x.val;
      tmp15++;
    }
    tmp14.len = tmp15;
    tmp8[tmp7] = (-_sum_int(tmp14));
    tmp7++;
  }
  tmp6.len = tmp7;
  for (int i19 = 0; i19 < tmp7 - 1; i19++) {
    for (int i20 = i19 + 1; i20 < tmp7; i20++) {
      if (tmp8[i19] > tmp8[i20]) {
        double tmp17 = tmp8[i19];
        tmp8[i19] = tmp8[i20];
        tmp8[i20] = tmp17;
        grouped_item_t tmp18 = tmp6.data[i19];
        tmp6.data[i19] = tmp6.data[i20];
        tmp6.data[i20] = tmp18;
      }
    }
  }
  grouped_item_list_t grouped = tmp6;
  for (int i21 = 0; i21 < grouped.len; i21++) {
    grouped_item_t it = grouped.data[i21];
    if (i21 > 0)
      printf(" ");
    printf("map[");
    printf("cat:");
    printf("%s", it.cat);
    printf(" ");
    printf("total:");
    printf("%.17g", it.total);
    printf("]");
  }
  printf("\n");
  return 0;
}
int main() { return _mochi_main(); }
