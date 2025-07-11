#include <stdio.h>
#include <stdlib.h>

typedef struct {
  int len;
  int *data;
} list_int;
static list_int list_int_create(int len) {
  list_int l;
  l.len = len;
  l.data = (int *)malloc(sizeof(int) * len);
  return l;
}
int main() {
  int _t1_data[] = {1, 2, 3, 4, 5, 6, 7, 8, 9};
  list_int _t1 = {9, _t1_data};
  list_int numbers = _t1;
  for (int _t2 = 0; _t2 < numbers.len; _t2++) {
    int n = numbers.data[_t2];
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
