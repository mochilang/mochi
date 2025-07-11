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
int main() {
  int tmp1_data[] = {10, 20, 30};
  list_int tmp1 = {3, tmp1_data};
  list_int xs = tmp1;
  printf("%.16g\n", xs.data[1]);
  return 0;
}
