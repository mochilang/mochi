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
list_int twoSum(list_int nums, int target) {
  int n = nums.len;
  for (int i = 0; i < n; i++) {
    for (int j = i + 1; j < n; j++) {
      if (nums.data[i] + nums.data[j] == target) {
        int tmp1_data[] = {i, j};
        list_int tmp1 = {2, tmp1_data};
        return tmp1;
      }
    }
  }
  int tmp2_data[] = {(-1), (-1)};
  list_int tmp2 = {2, tmp2_data};
  return tmp2;
}

int main() {
  int tmp3_data[] = {2, 7, 11, 15};
  list_int tmp3 = {4, tmp3_data};
  list_int result = twoSum(tmp3, 9);
  printf("%.16g\n", result.data[0]);
  printf("%.16g\n", result.data[1]);
  return 0;
}
