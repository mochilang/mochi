// Generated by Mochi compiler v0.10.28 on 2025-07-18T10:18:22Z
// Generated by Mochi compiler v0.10.28 on 2025-07-18T10:18:22Z
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
  int len;
  list_int *data;
} list_list_int;
static list_list_int list_list_int_create(int len) {
  list_list_int l;
  l.len = len;
  l.data = calloc(len, sizeof(list_int));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
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
static int cmp_map_string_int(map_string_int a, map_string_int b) {
  int aa = map_string_int_get(a, "a");
  int ba = map_string_int_get(b, "a");
  if (aa != ba)
    return aa > ba ? 1 : -1;
  int ab = map_string_int_get(a, "b");
  int bb = map_string_int_get(b, "b");
  if (ab != bb)
    return ab > bb ? 1 : -1;
  return 0;
}
static list_string concat_list_string(list_string a, list_string b) {
  list_string r = list_string_create(a.len + b.len);
  for (int i = 0; i < a.len; i++)
    r.data[i] = a.data[i];
  for (int i = 0; i < b.len; i++)
    r.data[a.len + i] = b.data[i];
  return r;
}
static char *concat_string(char *a, char *b) {
  size_t len1 = strlen(a);
  size_t len2 = strlen(b);
  char *buf = (char *)malloc(len1 + len2 + 1);
  memcpy(buf, a, len1);
  memcpy(buf + len1, b, len2);
  buf[len1 + len2] = '\0';
  return buf;
}
static char *_str(int v) {
  char *buf = (char *)malloc(32);
  sprintf(buf, "%d", v);
  return buf;
}
static char *slice_string(char *s, int start, int end) {
  int len = strlen(s);
  if (start < 0)
    start += len;
  if (end < 0)
    end += len;
  if (start < 0)
    start = 0;
  if (end > len)
    end = len;
  if (start > end)
    start = end;
  char *buf = (char *)malloc(end - start + 1);
  memcpy(buf, s + start, end - start);
  buf[end - start] = '\0';
  return buf;
}
static int i = 1;

char *join(list_string xs, char *sep) {
  char *res = "";
  int i = 0;
  while (i < xs.len) {
    if (i > 0) {
      char *tmp1 = concat_string(res, sep);
      res = tmp1;
    }
    char *tmp2 = concat_string(res, xs.data[i]);
    res = tmp2;
    i = i + 1;
  }
  return res;
}

int parseIntStr(char *str) {
  int i = 0;
  int neg = 0;
  char *tmp3 = slice_string(str, 0, 1);
  if ((str.len > 0 && tmp3 == "-")) {
    neg = 1;
    i = 1;
  }
  int n = 0;
  map_string_int tmp4 = map_string_int_create(10);
  map_string_int_put(&tmp4, "0", 0);
  map_string_int_put(&tmp4, "1", 1);
  map_string_int_put(&tmp4, "2", 2);
  map_string_int_put(&tmp4, "3", 3);
  map_string_int_put(&tmp4, "4", 4);
  map_string_int_put(&tmp4, "5", 5);
  map_string_int_put(&tmp4, "6", 6);
  map_string_int_put(&tmp4, "7", 7);
  map_string_int_put(&tmp4, "8", 8);
  map_string_int_put(&tmp4, "9", 9);
  map_string_int digits = tmp4;
  while ((i < str.len)) {
    char *tmp5 = slice_string(str, i, i + 1);
    n = n * 10 + map_string_int_get(digits, tmp5);
    i = i + 1;
  }
  if (neg) {
    n = (-n);
  }
  return n;
}

int _mochi_main() {
  list_string tmp6 = list_string_create(3);
  tmp6.data[0] = "A";
  tmp6.data[1] = "B";
  tmp6.data[2] = "C";
  list_string tmp7 = list_string_create(3);
  tmp7.data[0] = "1";
  tmp7.data[1] = "2";
  tmp7.data[2] = "3";
  list_string tmp8 = list_string_create(3);
  tmp8.data[0] = "4";
  tmp8.data[1] = "5";
  tmp8.data[2] = "6";
  list_string tmp9 = list_string_create(3);
  tmp9.data[0] = "7";
  tmp9.data[1] = "8";
  tmp9.data[2] = "9";
  list_int rows_data[] = {tmp6, tmp7, tmp8, tmp9};
  list_list_int rows = {4, rows_data};
  list_string tmp10 = list_string_create(1);
  tmp10.data[0] = "SUM";
  list_string tmp11 = concat_list_string(rows.data[0], tmp10);
  rows.data[0] = 0;
  while (i < rows.len) {
    int sum = 0;
    for (int tmp12 = 0; rows.data[i][tmp12] != '\0'; tmp12++) {
      char s[2];
      s[0] = rows.data[i][tmp12];
      s[1] = '\0';
      sum = sum + parseIntStr(s);
    }
    char *tmp13 = _str(sum);
    char *tmp14 = _str(sum);
    rows.data[i] = 0;
    i = i + 1;
  }
  for (int tmp15 = 0; tmp15 < rows.len; tmp15++) {
    char *r = rows.data[tmp15];
    printf("%d\n", join(r, ","));
  }
  return 0;
}
int main() { return _mochi_main(); }
