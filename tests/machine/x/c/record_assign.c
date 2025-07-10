#include <stdio.h>
#include <stdlib.h>

typedef struct Counter Counter;

typedef struct Counter {
  int n;
} Counter;

int inc(Counter *c) { c->n = c->n + 1; }

int main() {
  __auto_type c = (Counter){.n = 0};
  inc(&c);
  printf("%d\n", c.n);
  return 0;
}
