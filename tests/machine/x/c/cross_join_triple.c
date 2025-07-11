#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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
  char **data;
} list_string;
static list_string list_string_create(int len) {
  list_string l;
  l.len = len;
  l.data = (char **)malloc(sizeof(char *) * len);
  return l;
}
typedef struct {
  int n;
  char *l;
  int b;
} combosItem;
typedef struct {
  int len;
  combosItem *data;
} list_combosItem;
static list_combosItem list_combosItem_create(int len) {
  list_combosItem l;
  l.len = len;
  l.data = (combosItem *)malloc(sizeof(combosItem) * len);
  return l;
}

int main() {
  int _t1_data[] = {1, 2};
  list_int _t1 = {2, _t1_data};
  list_int nums = _t1;
  char *_t2_data[] = {"A", "B"};
  list_string _t2 = {2, _t2_data};
  list_string letters = _t2;
  int _t3_data[] = {1, 0};
  list_int _t3 = {2, _t3_data};
  list_int bools = _t3;
  list_combosItem _t4 =
      list_combosItem_create(nums.len * letters.len * bools.len);
  int _t5 = 0;
  for (int _t6 = 0; _t6 < nums.len; _t6++) {
    int n = nums.data[_t6];
    for (int _t7 = 0; _t7 < letters.len; _t7++) {
      char *l = letters.data[_t7];
      for (int _t8 = 0; _t8 < bools.len; _t8++) {
        int b = bools.data[_t8];
        _t4.data[_t5] = (combosItem){.n = n, .l = l, .b = b};
        _t5++;
      }
    }
  }
  _t4.len = _t5;
  list_combosItem combos = _t4;
  printf("%s\n", "--- Cross Join of three lists ---");
  for (int _t9 = 0; _t9 < combos.len; _t9++) {
    combosItem c = combos.data[_t9];
    printf("%d ", c.n);
    printf("%s ", c.l);
    printf("%s\n", (c.b) ? "true" : "false");
  }
  return 0;
}
