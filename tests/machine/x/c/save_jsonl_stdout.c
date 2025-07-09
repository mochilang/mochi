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
static void _write_string(FILE *f, const char *s) {
  fputc('"', f);
  for (const char *p = s; *p; p++) {
    if (*p == '"' || *p == '\\')
      fputc('\\', f);
    fputc(*p, f);
  }
  fputc('"', f);
}
static int _is_number(const char *s) {
  if (!*s)
    return 0;
  for (const char *p = s; *p; p++)
    if (!_isnum(*p))
      return 0;
  return 1;
}
static void _write_obj(FILE *f, map_string m) {
  fputc('{', f);
  for (int i = 0; i < m.len; i++) {
    if (i > 0)
      fputc(',', f);
    _write_string(f, m.data[i].key);
    fputc(':', f);
    if (_is_number(m.data[i].value))
      fputs(m.data[i].value, f);
    else
      _write_string(f, m.data[i].value);
  }
  fputc('}', f);
}
static void _save_json(list_map_string rows, const char *path) {
  FILE *f = (!path || path[0] == '\0' || strcmp(path, "-") == 0)
                ? stdout
                : fopen(path, "w");
  if (!f) {
    fprintf(stderr, "cannot open %s\n", path);
    exit(1);
  }
  if (rows.len == 1) {
    _write_obj(f, rows.data[0]);
  } else {
    fputc('[', f);
    for (int i = 0; i < rows.len; i++) {
      if (i > 0)
        fputc(',', f);
      _write_obj(f, rows.data[i]);
    }
    fputc(']', f);
  }
  if (f != stdout)
    fclose(f);
}
typedef struct {
  char *name;
  int age;
} peopleItem;
typedef struct {
  int len;
  peopleItem *data;
} list_peopleItem;
static list_peopleItem list_peopleItem_create(int len) {
  list_peopleItem l;
  l.len = len;
  l.data = (peopleItem *)malloc(sizeof(peopleItem) * len);
  return l;
}

int main() {
  list_peopleItem _t1 = list_peopleItem_create(2);
  _t1.data[0] = (peopleItem){.name = "Alice", .age = 30};
  _t1.data[1] = (peopleItem){.name = "Bob", .age = 25};
  list_peopleItem people = _t1;
  _save_json(people, "-");
  return 0;
}
