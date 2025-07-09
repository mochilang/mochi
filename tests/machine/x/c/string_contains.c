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
static int contains_string(char *s, char *sub) {
  return strstr(s, sub) != NULL;
}
int main() {
  char *s = "catch";
  printf("%s\n", (contains_string(s, "cat")) ? "true" : "false");
  printf("%s\n", (contains_string(s, "dog")) ? "true" : "false");
  return 0;
}
