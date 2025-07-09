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
static int contains_list_int(list_int v, int item) {
  for (int i = 0; i < v.len; i++)
    if (v.data[i] == item)
      return 1;
  return 0;
}
static int contains_string(char *s, char *sub) {
  return strstr(s, sub) != NULL;
}
int main() {
  list_int _t1 = list_int_create(3);
  _t1.data[0] = 1;
  _t1.data[1] = 2;
  _t1.data[2] = 3;
  list_int xs = _t1;
  list_int _t2 = list_int_create(xs.len);
  int _t3 = 0;
  for (int _t4 = 0; _t4 < xs.len; _t4++) {
    int x = xs.data[_t4];
    if (!(((x % 2) == 1))) {
      continue;
    }
    _t2.data[_t3] = x;
    _t3++;
  }
  _t2.len = _t3;
  list_int ys = _t2;
  printf("%s\n", (contains_list_int(ys, 1)) ? "true" : "false");
  printf("%s\n", (contains_list_int(ys, 2)) ? "true" : "false");
  map_int_bool _t5 = map_int_bool_create(1);
  map_int_bool_put(&_t5, "a", 1);
  int m = _t5;
  printf("%s\n", (("a" in m)) ? "true" : "false");
  printf("%s\n", (("b" in m)) ? "true" : "false");
  char *s = "hello";
  printf("%s\n", (contains_string(s, "ell")) ? "true" : "false");
  printf("%s\n", (contains_string(s, "foo")) ? "true" : "false");
  return 0;
}
