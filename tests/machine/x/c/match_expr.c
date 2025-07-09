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
int main() {
  int x = 2;
  char *label =
      (x == 1 ? "one" : (x == 2 ? "two" : (x == 3 ? "three" : "unknown")));
  printf("%s\n", label);
  return 0;
}
