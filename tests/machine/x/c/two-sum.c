#include <stdio.h>
#include <stdlib.h>

list_int twoSum(list_int nums, int target) {
  int n = nums.len;
  for (int i = 0; i < n; i++) {
    for (int j = i + 1; j < n; j++) {
      if (nums[i] + nums[j] == target) {
        int tmp1[] = {i, j};
        return tmp1;
      }
    }
  }
  int tmp2[] = {(-1), (-1)};
  return tmp2;
}

int main() {
  int tmp3[] = {2, 7, 11, 15};
  list_int result = twoSum(tmp3, 9);
  printf("%d\n", result[0]);
  printf("%d\n", result[1]);
  return 0;
}
