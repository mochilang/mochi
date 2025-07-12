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
static int contains_list_int(list_int v, int item) {
  for (int i = 0; i < v.len; i++)
    if (v.data[i] == item)
      return 1;
  return 0;
}
int main() {
  int nums[] = {1, 2, 3};
  printf("%s\n", (contains_list_int(nums, 2)) ? "true" : "false");
  printf("%s\n", (contains_list_int(nums, 4)) ? "true" : "false");
  return 0;
}
