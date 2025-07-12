#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static char *slice_string(char *s, int start, int end) {
  int len = strlen(s);
  if (start < 0)
    start += len;
  if (end < 0)
    end += len;
  if (start < 0)
    start = 0;
  if (end > len)
    end = len;
  if (start > end)
    start = end;
  char *buf = (char *)malloc(end - start + 1);
  memcpy(buf, s + start, end - start);
  buf[end - start] = '\0';
  return buf;
}
static char *prefix = "fore";
static char *s1 = "forest";
static char *s2 = "desert";

int main() {
  char *tmp1 = slice_string(s1, 0, strlen(prefix));
  printf("%s\n", ((strcmp(tmp1, prefix) == 0)) ? "true" : "false");
  char *tmp2 = slice_string(s2, 0, strlen(prefix));
  printf("%s\n", ((strcmp(tmp2, prefix) == 0)) ? "true" : "false");
  return 0;
}
