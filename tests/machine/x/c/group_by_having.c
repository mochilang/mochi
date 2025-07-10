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
  double *data;
} list_float;
static list_float list_float_create(int len) {
  list_float l;
  l.len = len;
  l.data = (double *)malloc(sizeof(double) * len);
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
  int len;
  list_int *data;
} list_list_int;
static list_list_int list_list_int_create(int len) {
  list_list_int l;
  l.len = len;
  l.data = (list_int *)malloc(sizeof(list_int) * len);
  return l;
}
static void _json_int(int v) { printf("%d", v); }
static void _json_float(double v) { printf("%g", v); }
static void _json_string(char *s) { printf("\"%s\"", s); }
static void _json_list_int(list_int v) {
  printf("[");
  for (int i = 0; i < v.len; i++) {
    if (i > 0)
      printf(",");
    _json_int(v.data[i]);
  }
  printf("]");
}
static void _json_list_float(list_float v) {
  printf("[");
  for (int i = 0; i < v.len; i++) {
    if (i > 0)
      printf(",");
    _json_float(v.data[i]);
  }
  printf("]");
}
static void _json_list_string(list_string v) {
  printf("[");
  for (int i = 0; i < v.len; i++) {
    if (i > 0)
      printf(",");
    _json_string(v.data[i]);
  }
  printf("]");
}
static void _json_list_list_int(list_list_int v) {
  printf("[");
  for (int i = 0; i < v.len; i++) {
    if (i > 0)
      printf(",");
    _json_list_int(v.data[i]);
  }
  printf("]");
}
typedef struct {
  char *key;
  list_int items;
} _GroupString;
typedef struct {
  int len;
  int cap;
  _GroupString *data;
} list_group_string;
static list_group_string _group_by_string(list_string src) {
  list_group_string res;
  res.len = 0;
  res.cap = 0;
  res.data = NULL;
  for (int i = 0; i < src.len; i++) {
    char *key = src.data[i];
    int idx = -1;
    for (int j = 0; j < res.len; j++)
      if (strcmp(res.data[j].key, key) == 0) {
        idx = j;
        break;
      }
    if (idx == -1) {
      if (res.len >= res.cap) {
        res.cap = res.cap ? res.cap * 2 : 4;
        res.data =
            (_GroupString *)realloc(res.data, sizeof(_GroupString) * res.cap);
      }
      res.data[res.len].key = key;
      res.data[res.len].items = list_int_create(0);
      idx = res.len++;
    }
    _GroupString *g = &res.data[idx];
    g->items.data =
        (int *)realloc(g->items.data, sizeof(int) * (g->items.len + 1));
    g->items.data[g->items.len++] = i;
  }
  return res;
}
typedef struct {
  char *name;
  char *city;
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

typedef struct {
  int city;
  int num;
} bigItem;
typedef struct {
  int len;
  bigItem *data;
} list_bigItem;
static list_bigItem list_bigItem_create(int len) {
  list_bigItem l;
  l.len = len;
  l.data = (bigItem *)malloc(sizeof(bigItem) * len);
  return l;
}

int main() {
  list_peopleItem _t1 = list_peopleItem_create(7);
  _t1.data[0] = (peopleItem){.name = "Alice", .city = "Paris"};
  _t1.data[1] = (peopleItem){.name = "Bob", .city = "Hanoi"};
  _t1.data[2] = (peopleItem){.name = "Charlie", .city = "Paris"};
  _t1.data[3] = (peopleItem){.name = "Diana", .city = "Hanoi"};
  _t1.data[4] = (peopleItem){.name = "Eve", .city = "Paris"};
  _t1.data[5] = (peopleItem){.name = "Frank", .city = "Hanoi"};
  _t1.data[6] = (peopleItem){.name = "George", .city = "Paris"};
  list_peopleItem people = _t1;
  list_peopleItem _t2 = list_peopleItem_create(people.len);
  list_string _t3 = list_string_create(people.len);
  int _t4 = 0;
  for (int i = 0; i < people.len; i++) {
    peopleItem p = people.data[i];
    _t2.data[_t4] = p;
    _t3.data[_t4] = p.city;
    _t4++;
  }
  _t2.len = _t4;
  _t3.len = _t4;
  list_group_string _t5 = _group_by_string(_t3);
  list_int _t6 = list_int_create(_t5.len);
  int _t7 = 0;
  for (int gi = 0; gi < _t5.len; gi++) {
    _GroupString _gp = _t5.data[gi];
    list_peopleItem _t8 = list_peopleItem_create(_gp.items.len);
    for (int j = 0; j < _gp.items.len; j++) {
      _t8.data[j] = _t2.data[_gp.items.data[j]];
    }
    _t8.len = _gp.items.len;
    struct {
      char *key;
      list_peopleItem items;
    } g = {_gp.key, _t8};
    _t6.data[_t7] = (bigItem){.city = g.key, .num = g.items.len};
    _t7++;
  }
  _t6.len = _t7;
  list_bigItem big = _t6;
  _json_int(big);
  return 0;
}
