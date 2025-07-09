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
int sum_rec(int n, int acc) {
  if ((n == 0)) {
    return acc;
  }
  return sum_rec((n - 1), (acc + n));
}

int main() {
  printf("%d\n", sum_rec(10, 0));
  return 0;
}
