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
  list_int _t1 = list_int_create(9);
  _t1.data[0] = 1;
  _t1.data[1] = 2;
  _t1.data[2] = 3;
  _t1.data[3] = 4;
  _t1.data[4] = 5;
  _t1.data[5] = 6;
  _t1.data[6] = 7;
  _t1.data[7] = 8;
  _t1.data[8] = 9;
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
