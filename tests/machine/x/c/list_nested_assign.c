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
int main() {
  list_list_int _t1 = list_list_int_create(2);
  list_int _t2 = list_int_create(2);
  _t2.data[0] = 1;
  _t2.data[1] = 2;
  _t1.data[0] = _t2;
  list_int _t3 = list_int_create(2);
  _t3.data[0] = 3;
  _t3.data[1] = 4;
  _t1.data[1] = _t3;
  list_list_int matrix = _t1;
  matrix.data[1].data[0] = 5;
  printf("%d\n", matrix.data[1].data[0]);
  return 0;
}
