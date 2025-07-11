#include <stdio.h>
#include <stdlib.h>

typedef struct Counter Counter;

typedef struct Counter {
  int n;
} Counter;
typedef struct {
  int len;
  Counter *data;
} list_Counter;
static list_Counter list_Counter_create(int len) {
  list_Counter l;
  l.len = len;
  l.data = (Counter *)malloc(sizeof(Counter) * len);
  return l;
}

int inc(Counter *c) { c->n = c->n + 1; }

int main() {
  Counter c = (Counter){.n = 0};
  inc(&c);
  printf("%d\n", c.n);
  return 0;
}
