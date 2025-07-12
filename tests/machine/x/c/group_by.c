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
static double _avg(list_int v) {
  if (v.len == 0)
    return 0;
  double sum = 0;
  for (int i = 0; i < v.len; i++)
    sum += v.data[i];
  return sum / v.len;
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
  int age;
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

typedef struct {
  char *city;
  int count;
  double avg_age;
} StatsItem;
typedef struct {
  int len;
  StatsItem *data;
} list_StatsItem;
static list_StatsItem list_StatsItem_create(int len) {
  list_StatsItem l;
  l.len = len;
  l.data = calloc(len, sizeof(StatsItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

int main() {
  PeopleItem tmp1_data[] = {
      (PeopleItem){.name = "Alice", .age = 30, .city = "Paris"},
      (PeopleItem){.name = "Bob", .age = 15, .city = "Hanoi"},
      (PeopleItem){.name = "Charlie", .age = 65, .city = "Paris"},
      (PeopleItem){.name = "Diana", .age = 45, .city = "Hanoi"},
      (PeopleItem){.name = "Eve", .age = 70, .city = "Paris"},
      (PeopleItem){.name = "Frank", .age = 22, .city = "Hanoi"}};
  list_PeopleItem tmp1 = {6, tmp1_data};
  list_PeopleItem people = tmp1;
  list_PeopleItem tmp2 = list_PeopleItem_create(people.len);
  list_string tmp3 = list_string_create(people.len);
  int tmp4 = 0;
  for (int i5 = 0; i5 < people.len; i5++) {
    PeopleItem person = people.data[i5];
    tmp2.data[tmp4] = person;
    tmp3.data[tmp4] = person.city;
    tmp4++;
  }
  tmp2.len = tmp4;
  tmp3.len = tmp4;
  list_group_string tmp6 = _group_by_string(tmp3);
  list_StatsItem tmp7 = list_StatsItem_create(tmp6.len);
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
    list_int tmp11 = list_int_create(g.items.len);
    int tmp12 = 0;
    for (int i13 = 0; i13 < g.items.len; i13++) {
      PeopleItem p = g.items.data[i13];
      tmp11.data[tmp12] = p.age;
      tmp12++;
    }
    tmp11.len = tmp12;
    tmp7.data[tmp8] = (StatsItem){
        .city = g.key, .count = g.items.len, .avg_age = _avg(tmp11)};
    tmp8++;
  }
  tmp7.len = tmp8;
  list_StatsItem stats = tmp7;
  printf("%s\n", "--- People grouped by city ---");
  // unsupported dynamic list iteration
  for (;;) {
    break;
  }
  return 0;
}
