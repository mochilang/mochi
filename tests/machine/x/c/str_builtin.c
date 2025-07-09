#include <stdio.h>
#include <stdlib.h>

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
