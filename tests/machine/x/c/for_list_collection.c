// Generated by Mochi compiler v0.10.28 on 2006-01-02T15:04:05Z
#include <stdio.h>
#include <stdlib.h>

typedef struct {
  int len;
  int *data;
} list_int;
static list_int list_int_create(int len) {
  list_int l;
  l.len = len;
  l.data = calloc(len, sizeof(int));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}
int _mochi_main() {
  list_int tmp1 = list_int_create(3);
  tmp1.data[0] = 1;
  tmp1.data[1] = 2;
  tmp1.data[2] = 3;
  for (int tmp2 = 0; tmp2 < 3; tmp2++) {
    int n = tmp1.data[tmp2];
    printf("%d\n", n);
  }
  return 0;
}
int main() { return _mochi_main(); }
