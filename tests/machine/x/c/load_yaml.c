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
  m.data = cap ? (pair_string *)malloc(sizeof(pair_string) * cap) : NULL;
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
  l.data = cap ? (map_string *)malloc(sizeof(map_string) * cap) : NULL;
  return l;
}
static void list_map_string_push(list_map_string *l, map_string m) {
  if (l->len >= l->cap) {
    l->cap = l->cap ? l->cap * 2 : 4;
    l->data = (map_string *)realloc(l->data, sizeof(map_string) * l->cap);
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
  int name;
  int email;
} adultsItem;
typedef struct {
  int len;
  adultsItem *data;
} list_adultsItem;
static list_adultsItem list_adultsItem_create(int len) {
  list_adultsItem l;
  l.len = len;
  l.data = (adultsItem *)malloc(sizeof(adultsItem) * len);
  return l;
}

typedef struct {
  char *name;
  int age;
  char *email;
} Person;

int main() {
  list_Person people = _load_json("../interpreter/valid/people.yaml");
  list_adultsItem _t1 = list_adultsItem_create(people.len);
  int _t2 = 0;
  for (int _t3 = 0; _t3 < people.len; _t3++) {
    Person p = people.data[_t3];
    if (!(p.age >= 18)) {
      continue;
    }
    _t1.data[_t2] = (adultsItem){.name = p.name, .email = p.email};
    _t2++;
  }
  _t1.len = _t2;
  list_adultsItem adults = _t1;
  for (int _t4 = 0; _t4 < adults.len; _t4++) {
    adultsItem a = adults.data[_t4];
    printf("%d ", a.name);
    printf("%d\n", a.email);
  }
  return 0;
}
