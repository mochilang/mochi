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
  int tmp1_data[] = {1, 2, 3, 4, 5, 6, 7, 8, 9};
  list_int tmp1 = {9, tmp1_data};
  list_int numbers = tmp1;
  for (int tmp2 = 0; tmp2 < numbers.len; tmp2++) {
    int n = numbers.data[tmp2];
    if (n % 2 == 0) {
      continue;
    }
    if (n > 7) {
      break;
    }
    printf("%s ", "odd number:");
    printf("%d\n", n);
  }
  return 0;
}
