#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static char *concat_string(char *a, char *b) {
  size_t len1 = strlen(a);
  size_t len2 = strlen(b);
  char *buf = (char *)malloc(len1 + len2 + 1);
  memcpy(buf, a, len1);
  memcpy(buf + len1, b, len2);
  buf[len1 + len2] = '\0';
  return buf;
}
int main() {
  char *tmp1 = concat_string("hello ", "world");
  printf("%s\n", tmp1);
  return 0;
}
