#include <stdio.h>
#include <stdlib.h>

static int _sum_int(list_int v) {
  int sum = 0;
  for (int i = 0; i < v.len; i++)
    sum += v.data[i];
  return sum;
}
int main() {
  int nums[] = {1, 2, 3};
  int tmp1[] = {1, 2, 3};
  list_int tmp2 = list_int_create(tmp1.len);
  int tmp3 = 0;
  for (int tmp4 = 0; tmp4 < tmp1.len; tmp4++) {
    int n = tmp1.data[tmp4];
    if (!(n > 1)) {
      continue;
    }
    tmp2.data[tmp3] = n;
    tmp3++;
  }
  tmp2.len = tmp3;
  double result = _sum_int(tmp2);
  printf("%.16g\n", result);
  return 0;
}
