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
  int key;
  int value;
} map_int_bool_item;
static map_int_bool_item *map_int_bool_item_new(int key, int value) {
  map_int_bool_item *it = calloc(1, sizeof(map_int_bool_item));
  if (!it) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  it->key = key;
  it->value = value;
  return it;
}
typedef struct {
  int len;
  int cap;
  map_int_bool_item **data;
} map_int_bool;
static map_int_bool map_int_bool_create(int cap) {
  map_int_bool m;
  m.len = 0;
  m.cap = cap;
  m.data = cap ? calloc(cap, sizeof(map_int_bool_item *)) : NULL;
  if (cap && !m.data) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return m;
}
static void map_int_bool_put(map_int_bool *m, int key, int value) {
  for (int i = 0; i < m->len; i++)
    if (m->data[i]->key == key) {
      m->data[i]->value = value;
      return;
    }
  if (m->len >= m->cap) {
    m->cap = m->cap ? m->cap * 2 : 4;
    m->data = (map_int_bool_item **)realloc(
        m->data, sizeof(map_int_bool_item *) * m->cap);
  }
  m->data[m->len++] = map_int_bool_item_new(key, value);
}
static int map_int_bool_contains(map_int_bool m, int key) {
  for (int i = 0; i < m.len; i++)
    if (m.data[i]->key == key)
      return 1;
  return 0;
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
} dataItem;
typedef struct {
  int len;
  dataItem *data;
} list_dataItem;
static list_dataItem list_dataItem_create(int len) {
  list_dataItem l;
  l.len = len;
  l.data = calloc(len, sizeof(dataItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

int main() {
  dataItem tmp1_data[] = {(dataItem){.tag = "a", .val = 1},
                          (dataItem){.tag = "a", .val = 2},
                          (dataItem){.tag = "b", .val = 3}};
  list_dataItem tmp1 = {3, tmp1_data};
  list_dataItem data = tmp1;
  list_dataItem tmp2 = list_dataItem_create(data.len);
  list_string tmp3 = list_string_create(data.len);
  int tmp4 = 0;
  for (int i = 0; i < data.len; i++) {
    dataItem d = data.data[i];
    tmp2.data[tmp4] = d;
    tmp3.data[tmp4] = d.tag;
    tmp4++;
  }
  tmp2.len = tmp4;
  tmp3.len = tmp4;
  list_group_string tmp5 = _group_by_string(tmp3);
  list_int tmp6 = list_int_create(tmp5.len);
  int tmp7 = 0;
  for (int gi = 0; gi < tmp5.len; gi++) {
    _GroupString _gp = tmp5.data[gi];
    list_dataItem tmp8 = list_dataItem_create(_gp.items.len);
    for (int j = 0; j < _gp.items.len; j++) {
      tmp8.data[j] = tmp2.data[_gp.items.data[j]];
    }
    tmp8.len = _gp.items.len;
    struct {
      char *key;
      list_dataItem items;
    } g = {_gp.key, tmp8};
    tmp6.data[tmp7] = g;
    tmp7++;
  }
  tmp6.len = tmp7;
  list_int groups = tmp6;
  int tmp9_data[] = {};
  list_int tmp9 = {0, tmp9_data};
  list_int tmp = tmp9;
  for (int tmp10 = 0; tmp10 < groups.len; tmp10++) {
    int g = groups.data[tmp10];
    int total = 0;
    for (int tmp11 = 0; tmp11 < g.items.len; tmp11++) {
      dataItem x = g.items.data[tmp11];
      total = total + x.val;
    }
    map_int_bool tmp12 = map_int_bool_create(2);
    map_int_bool_put(&tmp12, "tag", g.key);
    map_int_bool_put(&tmp12, "total", total);
    tmp = 0;
  }
  list_int tmp13 = list_int_create(tmp.len);
  int *tmp16 = (int *)malloc(sizeof(int) * tmp.len);
  int tmp14 = 0;
  for (int tmp15 = 0; tmp15 < tmp.len; tmp15++) {
    int r = tmp.data[tmp15];
    tmp13.data[tmp14] = r;
    tmp16[tmp14] = r.tag;
    tmp14++;
  }
  tmp13.len = tmp14;
  for (int i = 0; i < tmp14 - 1; i++) {
    for (int j = i + 1; j < tmp14; j++) {
      if (tmp16[i] > tmp16[j]) {
        int tmp17 = tmp16[i];
        tmp16[i] = tmp16[j];
        tmp16[j] = tmp17;
        int tmp18 = tmp13.data[i];
        tmp13.data[i] = tmp13.data[j];
        tmp13.data[j] = tmp18;
      }
    }
  }
  list_int result = tmp13;
  printf("%.16g\n", result);
  return 0;
}
