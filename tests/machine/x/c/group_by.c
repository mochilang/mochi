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
  char **data;
} list_string;
static list_string list_string_create(int len) {
  list_string l;
  l.len = len;
  l.data = (char **)malloc(sizeof(char *) * len);
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
  l.data = (peopleItem *)malloc(sizeof(peopleItem) * len);
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
  l.data = (statsItem *)malloc(sizeof(statsItem) * len);
  return l;
}

int main() {
  list_peopleItem _t1 = list_peopleItem_create(6);
  _t1.data[0] = (peopleItem){.name = "Alice", .age = 30, .city = "Paris"};
  _t1.data[1] = (peopleItem){.name = "Bob", .age = 15, .city = "Hanoi"};
  _t1.data[2] = (peopleItem){.name = "Charlie", .age = 65, .city = "Paris"};
  _t1.data[3] = (peopleItem){.name = "Diana", .age = 45, .city = "Hanoi"};
  _t1.data[4] = (peopleItem){.name = "Eve", .age = 70, .city = "Paris"};
  _t1.data[5] = (peopleItem){.name = "Frank", .age = 22, .city = "Hanoi"};
  __auto_type people = _t1;
  list_peopleItem _t2 = list_peopleItem_create(people.len);
  list_string _t3 = list_string_create(people.len);
  int _t4 = 0;
  for (int i = 0; i < people.len; i++) {
    peopleItem person = people.data[i];
    _t2.data[_t4] = person;
    _t3.data[_t4] = person.city;
    _t4++;
  }
  _t2.len = _t4;
  _t3.len = _t4;
  list_group_string _t5 = _group_by_string(_t3);
  list_statsItem _t6 = list_statsItem_create(_t5.len);
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
    list_int _t9 = list_int_create(g.items.len);
    int _t10 = 0;
    for (int i = 0; i < g.items.len; i++) {
      peopleItem p = g.items.data[i];
      _t9.data[_t10] = p.age;
      _t10++;
    }
    _t9.len = _t10;
    _t6.data[_t7] =
        (statsItem){.city = g.key, .count = g.items.len, .avg_age = _avg(_t9)};
    _t7++;
  }
  _t6.len = _t7;
  list_statsItem stats = _t6;
  printf("%s\n", "--- People grouped by city ---");
  for (int _t11 = 0; _t11 < stats.len; _t11++) {
    statsItem s = stats.data[_t11];
    printf("%s ", s.city);
    printf("%s ", ": count =");
    printf("%d ", s.count);
    printf("%s ", ", avg_age =");
    printf("%.17g\n", s.avg_age);
  }
  return 0;
}
