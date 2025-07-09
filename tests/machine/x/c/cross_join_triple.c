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
  int len;
  char **data;
} list_string;
static list_string list_string_create(int len) {
  list_string l;
  l.len = len;
  l.data = (char **)malloc(sizeof(char *) * len);
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
int main() {
  list_int _t1 = list_int_create(2);
  _t1.data[0] = 1;
  _t1.data[1] = 2;
  list_int nums = _t1;
  list_string _t2 = list_string_create(2);
  _t2.data[0] = "A";
  _t2.data[1] = "B";
  list_string letters = _t2;
  list_int _t3 = list_int_create(2);
  _t3.data[0] = 1;
  _t3.data[1] = 0;
  list_int bools = _t3;
  map_int_bool _t4 = map_int_bool_create(3);
  map_int_bool_put(&_t4, "n", n);
  map_int_bool_put(&_t4, "l", l);
  map_int_bool_put(&_t4, "b", b);
  list_int _t5 = list_int_create(nums.len * letters.len * bools.len);
  int _t6 = 0;
  for (int _t7 = 0; _t7 < nums.len; _t7++) {
    int n = nums.data[_t7];
    for (int _t8 = 0; _t8 < letters.len; _t8++) {
      char *l = letters.data[_t8];
      for (int _t9 = 0; _t9 < bools.len; _t9++) {
        int b = bools.data[_t9];
        _t5.data[_t6] = _t4;
        _t6++;
      }
    }
  }
  _t5.len = _t6;
  list_int combos = _t5;
  printf("%s\n", "--- Cross Join of three lists ---");
  for (int _t10 = 0; _t10 < combos.len; _t10++) {
    int c = combos.data[_t10];
    printf("%d ", c.n);
    printf("%d ", c.l);
    printf("%d\n", c.b);
  }
  return 0;
}
