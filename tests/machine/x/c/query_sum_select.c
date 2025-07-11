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
static int _sum_int(list_int v) {
  int sum = 0;
  for (int i = 0; i < v.len; i++)
    sum += v.data[i];
  return sum;
}
int main() {
  int tmp1_data[] = {1, 2, 3};
  list_int tmp1 = {3, tmp1_data};
  list_int nums = tmp1;
  list_int tmp2 = list_int_create(nums.len);
  int tmp3 = 0;
  for (int tmp4 = 0; tmp4 < nums.len; tmp4++) {
    int n = nums.data[tmp4];
    if (!(n > 1)) {
      continue;
    }
    tmp2.data[tmp3] = n;
    tmp3++;
  }
  tmp2.len = tmp3;
  double result = _sum_int(tmp2);
  printf("%.17g\n", result);
  return 0;
}
