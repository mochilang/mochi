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
static char *_read_all(const char *path) {
  FILE *f = (!path || path[0] == '\0' || strcmp(path, "-") == 0)
                ? stdin
                : fopen(path, "r");
  if (!f) {
    fprintf(stderr, "cannot open %s\n", path);
    exit(1);
  }
  fseek(f, 0, SEEK_END);
  long n = ftell(f);
  if (n < 0)
    n = 0;
  rewind(f);
  char *buf = (char *)malloc(n + 1);
  fread(buf, 1, n, f);
  buf[n] = '\0';
  if (f != stdin)
    fclose(f);
  return buf;
}
typedef struct {
  char *key;
  char *value;
} pair_string;
typedef struct {
  int len;
  int cap;
  pair_string *data;
} map_string;
static map_string map_string_create(int cap) {
  map_string m;
  m.len = 0;
  m.cap = cap;
  m.data = cap ? calloc(cap, sizeof(pair_string)) : NULL;
  if (cap && !m.data) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return m;
}
static void map_string_put(map_string *m, char *k, char *v) {
  if (m->len >= m->cap) {
    m->cap = m->cap ? m->cap * 2 : 4;
    m->data = (pair_string *)realloc(m->data, sizeof(pair_string) * m->cap);
  }
  m->data[m->len].key = k;
  m->data[m->len].value = v;
  m->len++;
}
typedef struct {
  int len;
  int cap;
  map_string *data;
} list_map_string;
static list_map_string list_map_string_create(int cap) {
  list_map_string l;
  l.len = 0;
  l.cap = cap;
  l.data = cap ? calloc(cap, sizeof(map_string)) : NULL;
  if (cap && !l.data) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}
static void list_map_string_push(list_map_string *l, map_string m) {
  if (l->len >= l->cap) {
    l->cap = l->cap ? l->cap * 2 : 4;
    l->data = (map_string *)realloc(l->data, sizeof(map_string) * l->cap);
  }
  l->data[l->len++] = m;
}
typedef struct {
  int len;
  int cap;
  map_string_int *data;
} list_map_string_int;
static list_map_string_int list_map_string_int_create(int cap) {
  list_map_string_int l;
  l.len = 0;
  l.cap = cap;
  l.data = cap ? calloc(cap, sizeof(map_string_int)) : NULL;
  if (cap && !l.data) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}
static void list_map_string_int_push(list_map_string_int *l, map_string_int m) {
  if (l->len >= l->cap) {
    l->cap = l->cap ? l->cap * 2 : 4;
    l->data =
        (map_string_int *)realloc(l->data, sizeof(map_string_int) * l->cap);
  }
  l->data[l->len++] = m;
}
static void _skip_ws(const char **s) {
  while (**s && (**s == ' ' || **s == '\t' || **s == '\n' || **s == '\r'))
    (*s)++;
}
static char *_parse_string(const char **s) {
  const char *p = *s;
  if (*p != '"')
    return strdup("");
  p++;
  const char *st = p;
  while (*p && *p != '"') {
    if (*p == '\\' && p[1])
      p++;
    p++;
  }
  size_t len = p - st;
  char *out = (char *)malloc(len + 1);
  memcpy(out, st, len);
  out[len] = '\0';
  if (*p == '"')
    p++;
  *s = p;
  return out;
}
static int _isnum(char c) {
  return (c >= '0' && c <= '9') || c == '-' || c == '+' || c == '.';
}
static char *_parse_token(const char **s) {
  const char *p = *s;
  const char *st = p;
  while (_isnum(*p))
    p++;
  size_t len = p - st;
  char *out = (char *)malloc(len + 1);
  memcpy(out, st, len);
  out[len] = '\0';
  *s = p;
  return out;
}
static map_string _parse_json_obj(const char **s) {
  const char *p = *s;
  if (*p == '{')
    p++;
  map_string row = map_string_create(0);
  _skip_ws(&p);
  while (*p && *p != '}') {
    _skip_ws(&p);
    char *k = _parse_string(&p);
    _skip_ws(&p);
    if (*p == ':')
      p++;
    _skip_ws(&p);
    char *v;
    if (*p == '"')
      v = _parse_string(&p);
    else
      v = _parse_token(&p);
    map_string_put(&row, k, v);
    _skip_ws(&p);
    if (*p == ',') {
      p++;
      _skip_ws(&p);
    }
  }
  if (*p == '}')
    p++;
  *s = p;
  return row;
}
static list_map_string _parse_json(const char *text) {
  const char *p = text;
  _skip_ws(&p);
  list_map_string rows = list_map_string_create(0);
  if (*p == '[') {
    p++;
    _skip_ws(&p);
    while (*p && *p != ']') {
      map_string r = _parse_json_obj(&p);
      list_map_string_push(&rows, r);
      _skip_ws(&p);
      if (*p == ',') {
        p++;
        _skip_ws(&p);
      }
    }
    if (*p == ']')
      p++;
  } else if (*p == '{') {
    map_string r = _parse_json_obj(&p);
    list_map_string_push(&rows, r);
  }
  return rows;
}
static list_map_string _load_json(const char *path) {
  char *text = _read_all(path);
  list_map_string rows = _parse_json(text);
  free(text);
  return rows;
}
typedef struct Person Person;

typedef struct {
  char *name;
  char *email;
} AdultsItem;
typedef struct {
  int len;
  AdultsItem *data;
} list_AdultsItem;
static list_AdultsItem list_AdultsItem_create(int len) {
  list_AdultsItem l;
  l.len = len;
  l.data = calloc(len, sizeof(AdultsItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

typedef struct Person {
  char *name;
  int age;
  char *email;
} Person;
typedef struct {
  int len;
  Person *data;
} list_Person;
static list_Person list_Person_create(int len) {
  list_Person l;
  l.len = len;
  l.data = calloc(len, sizeof(Person));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

int main() {
  list_Person people = _load_json("../interpreter/valid/people.yaml");
  list_adultsItem tmp1 = list_adultsItem_create(people.len);
  int tmp2 = 0;
  for (int tmp3 = 0; tmp3 < people.len; tmp3++) {
    Person p = people.data[tmp3];
    if (!(p.age >= 18)) {
      continue;
    }
    tmp1.data[tmp2] = (AdultsItem){.name = p.name, .email = p.email};
    tmp2++;
  }
  tmp1.len = tmp2;
  list_AdultsItem adults = tmp1;
  for (int tmp4 = 0; tmp4 < adults.len; tmp4++) {
    AdultsItem a = adults.data[tmp4];
    printf("%s ", a.name);
    printf("%s\n", a.email);
  }
  return 0;
}
