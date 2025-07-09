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
  int x = 8;
  char *msg = ((x > 10) ? "big" : ((x > 5) ? "medium" : "small"));
  printf("%s\n", msg);
  return 0;
}
