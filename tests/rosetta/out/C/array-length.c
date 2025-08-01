// Generated by Mochi compiler v0.10.30 on 2025-07-18T17:09:29Z
// Generated by Mochi compiler v0.10.30 on 2025-07-18T17:09:29Z
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
  int len;
  char **data;
} list_string;
static list_string list_string_create(int len) {
  list_string l;
  l.len = len;
  l.data = calloc(len, sizeof(char *));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}
static char *concat_string(char *a, char *b) {
  size_t len1 = strlen(a);
  size_t len2 = strlen(b);
  char *buf = (char *)malloc(len1 + len2 + 1);
  memcpy(buf, a, len1);
  memcpy(buf + len1, b, len2);
  buf[len1 + len2] = '\0';
  return buf;
}
static char *_str(int v) {
  char *buf = (char *)malloc(32);
  sprintf(buf, "%d", v);
  return buf;
}
int mochi_main() {
  list_string arr = list_string_create(3);
  arr.data[0] = "apple";
  arr.data[1] = "orange";
  arr.data[2] = "pear";
  char *tmp1 = _str(arr);
  char *tmp2 = concat_string("Length of ", tmp1);
  char *tmp3 = concat_string(tmp2, " is ");
  char *tmp4 = _str(3);
  char *tmp5 = concat_string(tmp3, tmp4);
  char *tmp6 = concat_string(tmp5, ".");
  char *tmp7 = _str(arr);
  char *tmp8 = concat_string("Length of ", tmp7);
  char *tmp9 = concat_string(tmp8, " is ");
  char *tmp10 = _str(3);
  char *tmp11 = concat_string(tmp9, tmp10);
  char *tmp12 = concat_string(tmp11, ".");
  printf("%d\n", tmp12);
}

int _mochi_main() {
  mochi_main();
  return 0;
}
int main() { return _mochi_main(); }
