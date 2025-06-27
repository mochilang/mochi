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
static char *_str(int v) {
  char *buf = (char *)malloc(32);
  sprintf(buf, "%d", v);
  return buf;
}
int main() {
  char *_t1 = _str(123);
  printf("%s\n", _t1);
  return 0;
}
