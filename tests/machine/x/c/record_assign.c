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
  l.data = calloc(len, sizeof(Counter));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}
static void _print_list_Counter(list_Counter v) {
  for (int i = 0; i < v.len; i++) {
    Counter s = v.data[i];
    printf("map[");
    printf("n:");
    printf("%d", s.n);
    printf(" ");
    printf("n:");
    printf("%d", s.n);
    printf("]");
    if (i < v.len - 1)
      printf(" ");
  }
}

int inc(Counter *c) { c->n = c->n + 1; }

int main() {
  Counter c = (Counter){.n = 0};
  inc(&c);
  printf("%d\n", c.n);
  return 0;
}
