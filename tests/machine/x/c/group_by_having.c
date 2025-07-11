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
  double *data;
} list_float;
static list_float list_float_create(int len) {
  list_float l;
  l.len = len;
  l.data = calloc(len, sizeof(double));
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
} PeopleItem;
typedef struct {
  int len;
  PeopleItem *data;
} list_PeopleItem;
static list_PeopleItem list_PeopleItem_create(int len) {
  list_PeopleItem l;
  l.len = len;
  l.data = calloc(len, sizeof(PeopleItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}
static void _print_list_peopleItem(list_PeopleItem v) {
  for (int i = 0; i < v.len; i++) {
    PeopleItem s = v.data[i];
    printf("map[");
    printf("name:");
    printf("%s", s.name);
    printf(" ");
    printf("city:");
    printf("%s", s.city);
    printf("]");
    if (i < v.len - 1)
      printf(" ");
  }
}

typedef struct {
  char *city;
  int num;
} BigItem;
typedef struct {
  int len;
  BigItem *data;
} list_BigItem;
static list_BigItem list_BigItem_create(int len) {
  list_BigItem l;
  l.len = len;
  l.data = calloc(len, sizeof(BigItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}
static void _print_list_bigItem(list_BigItem v) {
  for (int i = 0; i < v.len; i++) {
    BigItem s = v.data[i];
    printf("map[");
    printf("city:");
    printf("%s", s.city);
    printf(" ");
    printf("num:");
    printf("%d", s.num);
    printf("]");
    if (i < v.len - 1)
      printf(" ");
  }
}

int main() {
  PeopleItem tmp1_data[] = {(PeopleItem){.name = "Alice", .city = "Paris"},
                            (PeopleItem){.name = "Bob", .city = "Hanoi"},
                            (PeopleItem){.name = "Charlie", .city = "Paris"},
                            (PeopleItem){.name = "Diana", .city = "Hanoi"},
                            (PeopleItem){.name = "Eve", .city = "Paris"},
                            (PeopleItem){.name = "Frank", .city = "Hanoi"},
                            (PeopleItem){.name = "George", .city = "Paris"}};
  list_PeopleItem tmp1 = {7, tmp1_data};
  list_PeopleItem people = tmp1;
  list_PeopleItem tmp2 = list_PeopleItem_create(people.len);
  list_string tmp3 = list_string_create(people.len);
  int tmp4 = 0;
  for (int i5 = 0; i5 < people.len; i5++) {
    PeopleItem p = people.data[i5];
    tmp2.data[tmp4] = p;
    tmp3.data[tmp4] = p.city;
    tmp4++;
  }
  tmp2.len = tmp4;
  tmp3.len = tmp4;
  list_group_string tmp6 = _group_by_string(tmp3);
  list_BigItem tmp7 = list_BigItem_create(tmp6.len);
  int tmp8 = 0;
  for (int gi = 0; gi < tmp6.len; gi++) {
    _GroupString _gp = tmp6.data[gi];
    list_PeopleItem tmp9 = list_PeopleItem_create(_gp.items.len);
    for (int i10 = 0; i10 < _gp.items.len; i10++) {
      tmp9.data[i10] = tmp2.data[_gp.items.data[i10]];
    }
    tmp9.len = _gp.items.len;
    struct {
      char *key;
      list_PeopleItem items;
    } g = {_gp.key, tmp9};
    tmp7.data[tmp8] = (BigItem){.city = g.key, .num = g.items.len};
    tmp8++;
  }
  tmp7.len = tmp8;
  list_BigItem big = tmp7;
  _json_int(big);
  return 0;
}
