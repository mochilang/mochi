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
int main() {
  int a = (10 - 3);
  int b = (2 + 2);
  printf("%d\n", a);
  printf("%s\n", ((a == 7)) ? "true" : "false");
  printf("%s\n", ((b < 5)) ? "true" : "false");
  return 0;
}
