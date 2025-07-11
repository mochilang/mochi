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
} peopleItem;
typedef struct {
  int len;
  peopleItem *data;
} list_peopleItem;
static list_peopleItem list_peopleItem_create(int len) {
  list_peopleItem l;
  l.len = len;
  l.data = calloc(len, sizeof(peopleItem));
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
} statsItem;
typedef struct {
  int len;
  statsItem *data;
} list_statsItem;
static list_statsItem list_statsItem_create(int len) {
  list_statsItem l;
  l.len = len;
  l.data = calloc(len, sizeof(statsItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

int main() {
  peopleItem tmp1_data[] = {
      (peopleItem){.name = "Alice", .age = 30, .city = "Paris"},
      (peopleItem){.name = "Bob", .age = 15, .city = "Hanoi"},
      (peopleItem){.name = "Charlie", .age = 65, .city = "Paris"},
      (peopleItem){.name = "Diana", .age = 45, .city = "Hanoi"},
      (peopleItem){.name = "Eve", .age = 70, .city = "Paris"},
      (peopleItem){.name = "Frank", .age = 22, .city = "Hanoi"}};
  list_peopleItem tmp1 = {6, tmp1_data};
  list_peopleItem people = tmp1;
  list_peopleItem tmp2 = list_peopleItem_create(people.len);
  list_string tmp3 = list_string_create(people.len);
  int tmp4 = 0;
  for (int i = 0; i < people.len; i++) {
    peopleItem person = people.data[i];
    tmp2.data[tmp4] = person;
    tmp3.data[tmp4] = person.city;
    tmp4++;
  }
  tmp2.len = tmp4;
  tmp3.len = tmp4;
  list_group_string tmp5 = _group_by_string(tmp3);
  list_statsItem tmp6 = list_statsItem_create(tmp5.len);
  int tmp7 = 0;
  for (int gi = 0; gi < tmp5.len; gi++) {
    _GroupString _gp = tmp5.data[gi];
    list_peopleItem tmp8 = list_peopleItem_create(_gp.items.len);
    for (int j = 0; j < _gp.items.len; j++) {
      tmp8.data[j] = tmp2.data[_gp.items.data[j]];
    }
    tmp8.len = _gp.items.len;
    struct {
      char *key;
      list_peopleItem items;
    } g = {_gp.key, tmp8};
    list_int tmp9 = list_int_create(g.items.len);
    int tmp10 = 0;
    for (int i = 0; i < g.items.len; i++) {
      peopleItem p = g.items.data[i];
      tmp9.data[tmp10] = p.age;
      tmp10++;
    }
    tmp9.len = tmp10;
    tmp6.data[tmp7] =
        (statsItem){.city = g.key, .count = g.items.len, .avg_age = _avg(tmp9)};
    tmp7++;
  }
  tmp6.len = tmp7;
  list_statsItem stats = tmp6;
  printf("%s\n", "--- People grouped by city ---");
  for (int tmp11 = 0; tmp11 < stats.len; tmp11++) {
    statsItem s = stats.data[tmp11];
    printf("%s ", s.city);
    printf("%s ", ": count =");
    printf("%.16g ", s.count);
    printf("%s ", ", avg_age =");
    printf("%.16g\n", s.avg_age);
  }
  return 0;
}
