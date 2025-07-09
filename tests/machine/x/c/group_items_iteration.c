#include <stdio.h>
#include <stdlib.h>

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
  int key;
  int value;
} map_int_bool_item;
static map_int_bool_item *map_int_bool_item_new(int key, int value) {
  map_int_bool_item *it =
      (map_int_bool_item *)malloc(sizeof(map_int_bool_item));
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
  m.data = cap ? (map_int_bool_item **)malloc(sizeof(map_int_bool_item *) * cap)
               : NULL;
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
  l.data = (dataItem *)malloc(sizeof(dataItem) * len);
  return l;
}

int main() {
  list_dataItem _t1 = list_dataItem_create(3);
  _t1.data[0] = (dataItem){.tag = "a", .val = 1};
  _t1.data[1] = (dataItem){.tag = "a", .val = 2};
  _t1.data[2] = (dataItem){.tag = "b", .val = 3};
  list_dataItem data = _t1;
  list_int groups = 0;
  list_int tmp = _t2;
  for (int _t3 = 0; _t3 < groups.len; _t3++) {
    int g = groups.data[_t3];
    int total = 0;
    for (int _t4 = 0; _t4 < g.items.len; _t4++) {
      int x = g.items.data[_t4];
      total = (total + x.val);
    }
    map_int_bool _t5 = map_int_bool_create(2);
    map_int_bool_put(&_t5, "tag", g.key);
    map_int_bool_put(&_t5, "total", total);
    tmp = append(tmp, _t5);
  }
  list_int _t6 = list_int_create(tmp.len);
  int *_t9 = (int *)malloc(sizeof(int) * tmp.len);
  int _t7 = 0;
  for (int _t8 = 0; _t8 < tmp.len; _t8++) {
    int r = tmp.data[_t8];
    _t6.data[_t7] = r;
    _t9[_t7] = r.tag;
    _t7++;
  }
  _t6.len = _t7;
  for (int i = 0; i < _t7 - 1; i++) {
    for (int j = i + 1; j < _t7; j++) {
      if (_t9[i] > _t9[j]) {
        int _t10 = _t9[i];
        _t9[i] = _t9[j];
        _t9[j] = _t10;
        int _t11 = _t6.data[i];
        _t6.data[i] = _t6.data[j];
        _t6.data[j] = _t11;
      }
    }
  }
  list_int result = _t6;
  printf("%d\n", result);
  return 0;
}
