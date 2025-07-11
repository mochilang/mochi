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
  m.data = cap ? calloc(cap, sizeof(pair_string_int)) : NULL;
  if (cap && !m.data) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
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

int main() {
  DataItem tmp1_data[] = {(DataItem){.a = 1, .b = 2},
                          (DataItem){.a = 1, .b = 1},
                          (DataItem){.a = 0, .b = 5}};
  list_DataItem tmp1 = {3, tmp1_data};
  list_dataItem data = tmp1;
  list_dataItem tmp2 = list_dataItem_create(data.len);
  map_string_int *tmp5 =
      (map_string_int *)malloc(sizeof(map_string_int) * data.len);
  int tmp3 = 0;
  for (int tmp4 = 0; tmp4 < data.len; tmp4++) {
    dataItem x = data.data[tmp4];
    tmp2.data[tmp3] = x;
    map_string_int tmp6 = map_string_int_create(2);
    map_string_int_put(&tmp6, "a", x.a);
    map_string_int_put(&tmp6, "b", x.b);
    tmp5[tmp3] = tmp6;
    tmp3++;
  }
  tmp2.len = tmp3;
  for (int i = 0; i < tmp3 - 1; i++) {
    for (int j = i + 1; j < tmp3; j++) {
      if (tmp5[i] > tmp5[j]) {
        map_string_int tmp7 = tmp5[i];
        tmp5[i] = tmp5[j];
        tmp5[j] = tmp7;
        dataItem tmp8 = tmp2.data[i];
        tmp2.data[i] = tmp2.data[j];
        tmp2.data[j] = tmp8;
      }
    }
  }
  list_dataItem sorted = tmp2;
  printf("%d\n", sorted);
  return 0;
}
