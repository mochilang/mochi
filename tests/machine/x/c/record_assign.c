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
typedef struct Counter Counter;

typedef struct {
  int n;
} Counter;

int inc(Counter *c) { c.n = (c.n + 1); }

int main() {
  Counter c = (Counter){.n = 0};
  inc(&c);
  printf("%d\n", c.n);
  return 0;
}
