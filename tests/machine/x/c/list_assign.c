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
  int tmp1_data[] = {1, 2};
  list_int tmp1 = {2, tmp1_data};
  list_int nums = tmp1;
  nums.data[1] = 3;
  printf("%d\n", nums.data[1]);
  return 0;
}
