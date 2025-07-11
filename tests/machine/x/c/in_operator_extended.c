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
static int contains_list_int(list_int v, int item) {
  for (int i = 0; i < v.len; i++)
    if (v.data[i] == item)
      return 1;
  return 0;
}
static char *s = "hello";

int main() {
  int _t1_data[] = {1, 2, 3};
  list_int _t1 = {3, _t1_data};
  list_int xs = _t1;
  list_int _t2 = list_int_create(xs.len);
  int _t3 = 0;
  for (int _t4 = 0; _t4 < xs.len; _t4++) {
    int x = xs.data[_t4];
    if (!(x % 2 == 1)) {
      continue;
    }
    _t2.data[_t3] = x;
    _t3++;
  }
  _t2.len = _t3;
  list_int ys = _t2;
  printf("%s\n", (contains_list_int(ys, 1)) ? "true" : "false");
  printf("%s\n", (contains_list_int(ys, 2)) ? "true" : "false");
  map_string_int _t5 = map_string_int_create(1);
  map_string_int_put(&_t5, "a", 1);
  map_string_int m = _t5;
  printf("%s\n", (map_string_int_contains(m, "a")) ? "true" : "false");
  printf("%s\n", (map_string_int_contains(m, "b")) ? "true" : "false");
  printf("%d\n", "ell" in s);
  printf("%d\n", "foo" in s);
  return 0;
}
