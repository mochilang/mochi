#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static char *_index_string(char *s, int i) {
  int len = strlen(s);
  if (i < 0)
    i += len;
  if (i < 0 || i >= len) {
    fprintf(stderr, "index out of range\n");
    exit(1);
  }
  char *buf = (char *)malloc(2);
  buf[0] = s[i];
  buf[1] = '\0';
  return buf;
}
static char *s = "mochi";

int main() {
  char *tmp1 = _index_string(s, 1);
  printf("%s\n", tmp1);
  return 0;
}
