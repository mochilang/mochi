#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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
  printf("%s\n", ((strcmp("a", "b") < 0)) ? "true" : "false");
  printf("%s\n", ((strcmp("a", "a") <= 0)) ? "true" : "false");
  printf("%s\n", ((strcmp("b", "a") > 0)) ? "true" : "false");
  printf("%s\n", ((strcmp("b", "b") >= 0)) ? "true" : "false");
  return 0;
}
