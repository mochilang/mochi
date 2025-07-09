#include <stdio.h>
#include <stdlib.h>

typedef struct {
  int key;
  char *value;
} pair_int_string;
static pair_int_string pair_int_string_new(int key, char *val) {
  pair_int_string p;
  p.key = key;
  p.value = val;
  return p;
}
typedef struct {
  int len;
  int cap;
  pair_int_string *data;
} map_int_string;
static map_int_string map_int_string_create(int cap) {
  map_int_string m;
  m.len = 0;
  m.cap = cap;
  m.data =
      cap ? (pair_int_string *)malloc(sizeof(pair_int_string) * cap) : NULL;
  return m;
}
static void map_int_string_put(map_int_string *m, int k, char *v) {
  for (int i = 0; i < m->len; i++)
    if (m->data[i].key == k) {
      m->data[i].value = v;
      return;
    }
  if (m->len >= m->cap) {
    m->cap = m->cap ? m->cap * 2 : 4;
    m->data =
        (pair_int_string *)realloc(m->data, sizeof(pair_int_string) * m->cap);
  }
  m->data[m->len++] = pair_int_string_new(k, v);
}
static char *map_int_string_get(map_int_string m, int k) {
  for (int i = 0; i < m.len; i++)
    if (m.data[i].key == k)
      return m.data[i].value;
  return "";
}
static int map_int_string_contains(map_int_string m, int k) {
  for (int i = 0; i < m.len; i++)
    if (m.data[i].key == k)
      return 1;
  return 0;
}
int main() {
  map_int_string _t1 = map_int_string_create(2);
  map_int_string_put(&_t1, 1, "a");
  map_int_string_put(&_t1, 2, "b");
  map_int_string m = _t1;
  printf("%s\n", (map_int_string_contains(m, 1)) ? "true" : "false");
  printf("%s\n", (map_int_string_contains(m, 3)) ? "true" : "false");
  return 0;
}

