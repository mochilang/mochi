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
        list_int _t1 = list_int_create(2);
        _t1.data[0] = i;
        _t1.data[1] = j;
        return _t1;
      }
    }
  }
  list_int _t2 = list_int_create(2);
  _t2.data[0] = (-1);
  _t2.data[1] = (-1);
  return _t2;
}

int main() {
  list_int _t3 = list_int_create(4);
  _t3.data[0] = 2;
  _t3.data[1] = 7;
  _t3.data[2] = 11;
  _t3.data[3] = 15;
  list_int result = twoSum(_t3, 9);
  printf("%d\n", result.data[0]);
  printf("%d\n", result.data[1]);
  return 0;
}
