#include <stdio.h>
#include <stdlib.h>

typedef struct {
  int len;
  int cap;
  int *data;
} list_int;
static list_int list_int_create(int len) {
  list_int l;
  l.len = len;
  l.cap = len;
  l.data = len ? (int *)malloc(sizeof(int) * len) : NULL;
  return l;
}
static void list_int_free(list_int *l) {
  free(l->data);
  l->data = NULL;
  l->len = 0;
  l->cap = 0;
}
typedef struct {
  int len;
  int cap;
  list_int *data;
} list_list_int;
static list_list_int list_list_int_create(int len) {
  list_list_int l;
  l.len = len;
  l.cap = len;
  l.data = len ? (list_int *)malloc(sizeof(list_int) * len) : NULL;
  return l;
}
static void list_list_int_free(list_list_int *l) {
  for (int i = 0; i < l->len; i++)
    list_int_free(&l->data[i]);
  free(l->data);
  l->data = NULL;
  l->len = 0;
  l->cap = 0;
}
static list_int concat_list_int(list_int a, list_int b) {
  list_int r = list_int_create(a.len + b.len);
  for (int i = 0; i < a.len; i++)
    r.data[i] = a.data[i];
  for (int i = 0; i < b.len; i++)
    r.data[a.len + i] = b.data[i];
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
  list_int a = _t1;
  list_int _t2 = list_int_create(1);
  _t2.data[0] = 3;
  list_int _t3 = concat_list_int(a, _t2);
  _print_list_int(_t3);
  printf("\n");
  return 0;
}
