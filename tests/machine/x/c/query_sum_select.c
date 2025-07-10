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
  list_int _t1 = list_int_create(3);
  _t1.data[0] = 1;
  _t1.data[1] = 2;
  _t1.data[2] = 3;
  list_int nums = _t1;
  list_int _t2 = list_int_create(nums.len);
  int _t3 = 0;
  for (int _t4 = 0; _t4 < nums.len; _t4++) {
    int n = nums.data[_t4];
    if (!(n > 1)) {
      continue;
    }
    _t2.data[_t3] = n;
    _t3++;
  }
  _t2.len = _t3;
  double result = ({
    double sum = 0;
    for (int i = 0; i < _t2.len; i++)
      sum += _t2.data[i];
    sum;
  });
  printf("%.17g\n", result);
  return 0;
}
