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
static int contains_string(char *s, char *sub) {
  return strstr(s, sub) != NULL;
}
int main() {
  char *s = "catch";
  printf("%s\n", (contains_string(s, "cat")) ? "true" : "false");
  printf("%s\n", (contains_string(s, "dog")) ? "true" : "false");
  return 0;
}
