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
list_int twoSum(list_int nums, int target) {
  int n = nums.len;
  for (int i = 0; i < n; i++) {
    for (int j = i + 1; j < n; j++) {
      if (nums.data[i] + nums.data[j] == target) {
        int _t1_data[] = {i, j};
        list_int _t1 = {2, _t1_data};
        return _t1;
      }
    }
  }
  int _t2_data[] = {(-1), (-1)};
  list_int _t2 = {2, _t2_data};
  return _t2;
}

int main() {
  int _t3_data[] = {2, 7, 11, 15};
  list_int _t3 = {4, _t3_data};
  list_int result = twoSum(_t3, 9);
  printf("%d\n", result.data[0]);
  printf("%d\n", result.data[1]);
  return 0;
}
