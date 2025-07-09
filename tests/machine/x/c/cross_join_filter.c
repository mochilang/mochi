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
  list_int _t1 = list_int_create(3);
  _t1.data[0] = 1;
  _t1.data[1] = 2;
  _t1.data[2] = 3;
  list_int nums = _t1;
  list_string _t2 = list_string_create(2);
  _t2.data[0] = "A";
  _t2.data[1] = "B";
  list_string letters = _t2;
  map_int_bool _t3 = map_int_bool_create(2);
  map_int_bool_put(&_t3, "n", n);
  map_int_bool_put(&_t3, "l", l);
  list_int _t4 = list_int_create(nums.len * letters.len);
  int _t5 = 0;
  for (int _t6 = 0; _t6 < nums.len; _t6++) {
    int n = nums.data[_t6];
    for (int _t7 = 0; _t7 < letters.len; _t7++) {
      char *l = letters.data[_t7];
      if (!(((n % 2) == 0))) {
        continue;
      }
      _t4.data[_t5] = _t3;
      _t5++;
    }
  }
  _t4.len = _t5;
  list_int pairs = _t4;
  printf("%s\n", "--- Even pairs ---");
  for (int _t8 = 0; _t8 < pairs.len; _t8++) {
    int p = pairs.data[_t8];
    printf("%d ", p.n);
    printf("%d\n", p.l);
  }
  return 0;
}
