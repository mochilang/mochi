#include <stdio.h>
#include <stdlib.h>

typedef struct {
  int len;
  int cap;
  int *data;
} list_int;
static list_int list_int_create(int len) {
  list_int l;
  l.len = len;
  l.cap = len;
  l.data = len ? (int *)malloc(sizeof(int) * len) : NULL;
  return l;
}
static void list_int_free(list_int *l) {
  free(l->data);
  l->data = NULL;
  l->len = 0;
  l->cap = 0;
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
  int a;
  int b;
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
  _t1.data[0] = (dataItem){.a = 1, .b = 2};
  _t1.data[1] = (dataItem){.a = 1, .b = 1};
  _t1.data[2] = (dataItem){.a = 0, .b = 5};
  list_dataItem data = _t1;
  map_int_bool _t2 = map_int_bool_create(2);
  map_int_bool_put(&_t2, "a", x.a);
  map_int_bool_put(&_t2, "b", x.b);
  list_dataItem _t3 = list_dataItem_create(data.len);
  int *_t6 = (int *)malloc(sizeof(int) * data.len);
  int _t4 = 0;
  for (int _t5 = 0; _t5 < data.len; _t5++) {
    dataItem x = data.data[_t5];
    _t3.data[_t4] = x;
    _t6[_t4] = _t2;
    _t4++;
  }
  _t3.len = _t4;
  for (int i = 0; i < _t4 - 1; i++) {
    for (int j = i + 1; j < _t4; j++) {
      if (_t6[i] > _t6[j]) {
        int _t7 = _t6[i];
        _t6[i] = _t6[j];
        _t6[j] = _t7;
        dataItem _t8 = _t3.data[i];
        _t3.data[i] = _t3.data[j];
        _t3.data[j] = _t8;
      }
    }
  }
  list_dataItem sorted = _t3;
  printf("%d\n", sorted);
  return 0;
}
