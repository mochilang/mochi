// Generated by Mochi compiler v0.10.30 on 2025-07-18T17:11:45Z
// Generated by Mochi compiler v0.10.30 on 2025-07-18T17:11:45Z
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
static char *concat_string(char *a, char *b) {
  size_t len1 = strlen(a);
  size_t len2 = strlen(b);
  char *buf = (char *)malloc(len1 + len2 + 1);
  memcpy(buf, a, len1);
  memcpy(buf + len1, b, len2);
  buf[len1 + len2] = '\0';
  return buf;
}
static char *_input() {
  char buf[1024];
  if (!fgets(buf, sizeof(buf), stdin))
    return strdup("");
  size_t len = strlen(buf);
  if (len > 0 && buf[len - 1] == '\n')
    buf[len - 1] = '\0';
  return strdup(buf);
}
static char *_str(int v) {
  char *buf = (char *)malloc(32);
  sprintf(buf, "%d", v);
  return buf;
}
typedef struct some_struct_t some_struct_t;

typedef struct some_struct_t {
  int runtimeFields;
} some_struct_t;
typedef struct {
  int len;
  some_struct_t *data;
} some_struct_list_t;
some_struct_list_t create_some_struct_list(int len) {
  some_struct_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(some_struct_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

int mochi_main() {
  map_string_int tmp1 = map_string_int_create(0);
  __auto_type ss = (some_struct_t){.runtime_fields = tmp1};
  printf("Create two fields at runtime: \n\n");
  __auto_type i = 1;
  while (i <= 2) {
    char *tmp2 = _str(i);
    char *tmp3 = concat_string("  Field #", tmp2);
    char *tmp4 = concat_string(tmp3, ":\n");
    printf("%s\n", tmp4);
    printf("       Enter name  : \n");
    __auto_type name = _input();
    printf("       Enter value : \n");
    __auto_type value = _input();
    __auto_type fields = ss.runtime_fields;
    fields.data[name] = value;
    ss.runtime_fields = fields;
    printf("\n\n");
    i = i + 1;
  }
  while (1) {
    printf("Which field do you want to inspect ? \n");
    __auto_type name = _input();
    if (name in ss.runtime_fields) {
      __auto_type value = ss.runtime_fields.data[name];
      char *tmp5 = concat_string("Its value is '", value);
      char *tmp6 = concat_string(tmp5, "'");
      printf("%s\n", tmp6);
      return;
    } else {
      printf("There is no field of that name, try again\n\n");
    }
  }
}

int _mochi_main() {
  mochi_main();
  return 0;
}
int main() { return _mochi_main(); }
