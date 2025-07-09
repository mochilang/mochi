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
static void test_addition_works() {
  int x = (1 + 2);
  if (!((x == 3))) {
    fprintf(stderr, "expect failed\n");
    exit(1);
  }
}

int main() {
  printf("%s\n", "ok");
  test_addition_works();
  return 0;
}
