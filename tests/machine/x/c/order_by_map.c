#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
  char *key;
  int value;
} pair_string_int;
static pair_string_int pair_string_int_new(char *key, int value) {
  pair_string_int p;
  p.key = key;
  p.value = value;
  return p;
}
typedef struct {
  int len;
  int cap;
  pair_string_int *data;
} map_string_int;
static map_string_int map_string_int_create(int cap) {
  map_string_int m;
  m.len = 0;
  m.cap = cap;
  m.data =
      cap ? (pair_string_int *)malloc(sizeof(pair_string_int) * cap) : NULL;
  return m;
}
static void map_string_int_put(map_string_int *m, char *k, int v) {
  for (int i = 0; i < m->len; i++)
    if (strcmp(m->data[i].key, k) == 0) {
      m->data[i].value = v;
      return;
    }
  if (m->len >= m->cap) {
    m->cap = m->cap ? m->cap * 2 : 4;
    m->data =
        (pair_string_int *)realloc(m->data, sizeof(pair_string_int) * m->cap);
  }
  m->data[m->len++] = pair_string_int_new(k, v);
}
static int map_string_int_get(map_string_int m, const char *k) {
  for (int i = 0; i < m.len; i++)
    if (strcmp(m.data[i].key, k) == 0)
      return m.data[i].value;
  return 0;
}
static int map_string_int_contains(map_string_int m, const char *k) {
  for (int i = 0; i < m.len; i++)
    if (strcmp(m.data[i].key, k) == 0)
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
  list_dataItem _t2 = list_dataItem_create(data.len);
  map_string_int *_t5 =
      (map_string_int *)malloc(sizeof(map_string_int) * data.len);
  int _t3 = 0;
  for (int _t4 = 0; _t4 < data.len; _t4++) {
    dataItem x = data.data[_t4];
    _t2.data[_t3] = x;
    map_string_int _t6 = map_string_int_create(2);
    map_string_int_put(&_t6, "a", x.a);
    map_string_int_put(&_t6, "b", x.b);
    _t5[_t3] = _t6;
    _t3++;
  }
  _t2.len = _t3;
  for (int i = 0; i < _t3 - 1; i++) {
    for (int j = i + 1; j < _t3; j++) {
      if (_t5[i] > _t5[j]) {
        map_string_int _t7 = _t5[i];
        _t5[i] = _t5[j];
        _t5[j] = _t7;
        dataItem _t8 = _t2.data[i];
        _t2.data[i] = _t2.data[j];
        _t2.data[j] = _t8;
      }
    }
  }
  list_dataItem sorted = _t2;
  printf("%d\n", sorted);
  return 0;
}
