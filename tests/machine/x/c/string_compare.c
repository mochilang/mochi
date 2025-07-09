#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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
int main() {
  printf("%d\n", (strcmp("a", "b") < 0));
  printf("%d\n", (strcmp("a", "a") <= 0));
  printf("%d\n", (strcmp("b", "a") > 0));
  printf("%d\n", (strcmp("b", "b") >= 0));
  return 0;
}
