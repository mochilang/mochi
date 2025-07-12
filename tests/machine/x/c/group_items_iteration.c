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
typedef struct {
  int len;
  list_int *data;
} list_list_int;
static list_list_int list_list_int_create(int len) {
  list_list_int l;
  l.len = len;
  l.data = calloc(len, sizeof(list_int));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}
static void _print_list_int(list_int v) {
  for (int i = 0; i < v.len; i++) {
    if (i > 0)
      printf(" ");
    printf("%d", v.data[i]);
  }
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
  char *tag;
  int val;
} DataItem;
typedef struct {
  int len;
  DataItem *data;
} list_DataItem;
static list_DataItem list_DataItem_create(int len) {
  list_DataItem l;
  l.len = len;
  l.data = calloc(len, sizeof(DataItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

int main() {
  DataItem tmp1_data[] = {(DataItem){.tag = "a", .val = 1},
                          (DataItem){.tag = "a", .val = 2},
                          (DataItem){.tag = "b", .val = 3}};
  list_DataItem tmp1 = {3, tmp1_data};
  list_DataItem data = tmp1;
  list_DataItem tmp2 = list_DataItem_create(data.len);
  list_string tmp3 = list_string_create(data.len);
  int tmp4 = 0;
  for (int i5 = 0; i5 < data.len; i5++) {
    DataItem d = data.data[i5];
    tmp2.data[tmp4] = d;
    tmp3.data[tmp4] = d.tag;
    tmp4++;
  }
  tmp2.len = tmp4;
  tmp3.len = tmp4;
  list_group_string tmp6 = _group_by_string(tmp3);
  list_int tmp7 = list_int_create(tmp6.len);
  int tmp8 = 0;
  for (int gi = 0; gi < tmp6.len; gi++) {
    _GroupString _gp = tmp6.data[gi];
    list_DataItem tmp9 = list_DataItem_create(_gp.items.len);
    for (int i10 = 0; i10 < _gp.items.len; i10++) {
      tmp9.data[i10] = tmp2.data[_gp.items.data[i10]];
    }
    tmp9.len = _gp.items.len;
    struct {
      char *key;
      list_DataItem items;
    } g = {_gp.key, tmp9};
    tmp7.data[tmp8] = g;
    tmp8++;
  }
  tmp7.len = tmp8;
  list_int groups = tmp7;
  int tmp[] = {};
  // unsupported dynamic list iteration
  for (;;) {
    break;
  }
  int tmp12[] = {};
  list_int tmp13 = list_int_create(tmp12.len);
  int *tmp16 = (int *)malloc(sizeof(int) * tmp12.len);
  int tmp14 = 0;
  for (int tmp15 = 0; tmp15 < tmp12.len; tmp15++) {
    int r = tmp12.data[tmp15];
    tmp13.data[tmp14] = r;
    tmp16[tmp14] = r.tag;
    tmp14++;
  }
  tmp13.len = tmp14;
  for (int i19 = 0; i19 < tmp14 - 1; i19++) {
    for (int i20 = i19 + 1; i20 < tmp14; i20++) {
      if (tmp16[i19] > tmp16[i20]) {
        int tmp17 = tmp16[i19];
        tmp16[i19] = tmp16[i20];
        tmp16[i20] = tmp17;
        int tmp18 = tmp13.data[i19];
        tmp13.data[i19] = tmp13.data[i20];
        tmp13.data[i20] = tmp18;
      }
    }
  }
  list_int result = tmp13;
  _print_list_int(result);
  printf("\n");
  return 0;
}
