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
  char *s = "catch";
  printf("%s\n", (s.contains("cat")) ? "true" : "false");
  printf("%s\n", (s.contains("dog")) ? "true" : "false");
  return 0;
}
