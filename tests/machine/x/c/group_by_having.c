#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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
  list_bigItem big = 0;
  _json_int(big);
  return 0;
}
