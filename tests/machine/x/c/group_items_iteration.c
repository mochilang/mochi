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
static void _print_list_dataItem(list_DataItem v) {
  for (int i = 0; i < v.len; i++) {
    DataItem s = v.data[i];
    printf("map[");
    printf("tag:");
    printf("%s", s.tag);
    printf(" ");
    printf("val:");
    printf("%d", s.val);
    printf("]");
    if (i < v.len - 1)
      printf(" ");
  }
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
  int tmp11_data[] = {};
  list_int tmp11 = {0, tmp11_data};
  list_int tmp = tmp11;
  for (int tmp12 = 0; tmp12 < groups.len; tmp12++) {
    int g = groups.data[tmp12];
    int total = 0;
    for (int tmp13 = 0; tmp13 < g.items.len; tmp13++) {
      DataItem x = g.items.data[tmp13];
      total = total + x.val;
    }
    map_int_bool tmp14 = map_int_bool_create(2);
    map_int_bool_put(&tmp14, "tag", g.key);
    map_int_bool_put(&tmp14, "total", total);
    tmp = 0;
  }
  list_int tmp15 = list_int_create(tmp.len);
  int *tmp18 = (int *)malloc(sizeof(int) * tmp.len);
  int tmp16 = 0;
  for (int tmp17 = 0; tmp17 < tmp.len; tmp17++) {
    int r = tmp.data[tmp17];
    tmp15.data[tmp16] = r;
    tmp18[tmp16] = r.tag;
    tmp16++;
  }
  tmp15.len = tmp16;
  for (int i21 = 0; i21 < tmp16 - 1; i21++) {
    for (int i22 = i21 + 1; i22 < tmp16; i22++) {
      if (tmp18[i21] > tmp18[i22]) {
        int tmp19 = tmp18[i21];
        tmp18[i21] = tmp18[i22];
        tmp18[i22] = tmp19;
        int tmp20 = tmp15.data[i21];
        tmp15.data[i21] = tmp15.data[i22];
        tmp15.data[i22] = tmp20;
      }
    }
  }
  list_int result = tmp15;
  printf("%d\n", result);
  return 0;
}
