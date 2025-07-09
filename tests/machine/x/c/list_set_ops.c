#include <stdio.h>
#include <stdlib.h>

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
  list_int *data;
} list_list_int;
static list_list_int list_list_int_create(int len) {
  list_list_int l;
  l.len = len;
  l.data = (list_int *)malloc(sizeof(list_int) * len);
  return l;
}
static list_int concat_list_int(list_int a, list_int b) {
  list_int r = list_int_create(a.len + b.len);
  for (int i = 0; i < a.len; i++)
    r.data[i] = a.data[i];
  for (int i = 0; i < b.len; i++)
    r.data[a.len + i] = b.data[i];
  return r;
}
static list_int union_list_int(list_int a, list_int b) {
  list_int r = list_int_create(a.len + b.len);
  int idx = 0;
  for (int i = 0; i < a.len; i++) {
    int found = 0;
    for (int j = 0; j < idx; j++)
      if (r.data[j] == a.data[i]) {
        found = 1;
        break;
      }
    if (!found)
      r.data[idx++] = a.data[i];
  }
  for (int i = 0; i < b.len; i++) {
    int found = 0;
    for (int j = 0; j < idx; j++)
      if (r.data[j] == b.data[i]) {
        found = 1;
        break;
      }
    if (!found)
      r.data[idx++] = b.data[i];
  }
  r.len = idx;
  return r;
}
static list_int except_list_int(list_int a, list_int b) {
  list_int r = list_int_create(a.len);
  int idx = 0;
  for (int i = 0; i < a.len; i++) {
    int found = 0;
    for (int j = 0; j < b.len; j++)
      if (a.data[i] == b.data[j]) {
        found = 1;
        break;
      }
    if (!found)
      r.data[idx++] = a.data[i];
  }
  r.len = idx;
  return r;
}
static list_int intersect_list_int(list_int a, list_int b) {
  list_int r = list_int_create(a.len);
  int idx = 0;
  for (int i = 0; i < a.len; i++) {
    int found = 0;
    for (int j = 0; j < b.len; j++)
      if (a.data[i] == b.data[j]) {
        found = 1;
        break;
      }
    if (found) {
      int dup = 0;
      for (int j = 0; j < idx; j++)
        if (r.data[j] == a.data[i]) {
          dup = 1;
          break;
        }
      if (!dup)
        r.data[idx++] = a.data[i];
    }
  }
  r.len = idx;
  return r;
}
static void _print_list_int(list_int v) {
  for (int i = 0; i < v.len; i++) {
    if (i > 0)
      printf(" ");
    printf("%d", v.data[i]);
  }
}
int main() {
  list_int _t1 = list_int_create(2);
  _t1.data[0] = 1;
  _t1.data[1] = 2;
  list_int _t2 = list_int_create(2);
  _t2.data[0] = 2;
  _t2.data[1] = 3;
  list_int _t3 = union_list_int(_t1, _t2);
  _print_list_int(_t3);
  printf("\n");
  list_int _t4 = list_int_create(3);
  _t4.data[0] = 1;
  _t4.data[1] = 2;
  _t4.data[2] = 3;
  list_int _t5 = list_int_create(1);
  _t5.data[0] = 2;
  list_int _t6 = except_list_int(_t4, _t5);
  _print_list_int(_t6);
  printf("\n");
  list_int _t7 = list_int_create(3);
  _t7.data[0] = 1;
  _t7.data[1] = 2;
  _t7.data[2] = 3;
  list_int _t8 = list_int_create(2);
  _t8.data[0] = 2;
  _t8.data[1] = 4;
  list_int _t9 = intersect_list_int(_t7, _t8);
  _print_list_int(_t9);
  printf("\n");
  list_int _t10 = list_int_create(2);
  _t10.data[0] = 1;
  _t10.data[1] = 2;
  list_int _t11 = list_int_create(2);
  _t11.data[0] = 2;
  _t11.data[1] = 3;
  list_int _t12 = concat_list_int(_t10, _t11);
  printf("%d\n", _t12.len);
  return 0;
}
