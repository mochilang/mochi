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
typedef struct Counter Counter;

typedef struct {
  int n;
} Counter;

int inc(Counter *c) { c.n = c.n + 1; }

int main() {
  Counter c = (Counter){.n = 0};
  inc(&c);
  printf("%d\n", c.n);
  return 0;
}
