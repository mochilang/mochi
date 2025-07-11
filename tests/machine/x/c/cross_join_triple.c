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
  l.data = calloc(len, sizeof(int));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}
typedef struct {
  int len;
  char **data;
} list_string;
static list_string list_string_create(int len) {
  list_string l;
  l.len = len;
  l.data = calloc(len, sizeof(char *));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}
typedef struct {
  int n;
  char *l;
  int b;
} CombosItem;
typedef struct {
  int len;
  CombosItem *data;
} list_CombosItem;
static list_CombosItem list_CombosItem_create(int len) {
  list_CombosItem l;
  l.len = len;
  l.data = calloc(len, sizeof(CombosItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

int main() {
  int tmp1_data[] = {1, 2};
  list_int tmp1 = {2, tmp1_data};
  list_int nums = tmp1;
  char *tmp2_data[] = {"A", "B"};
  list_string tmp2 = {2, tmp2_data};
  list_string letters = tmp2;
  int tmp3_data[] = {1, 0};
  list_int tmp3 = {2, tmp3_data};
  list_int bools = tmp3;
  list_combosItem tmp4 =
      list_combosItem_create(nums.len * letters.len * bools.len);
  int tmp5 = 0;
  for (int n_idx = 0; n_idx < nums.len; n_idx++) {
    int n = nums.data[n_idx];
    for (int l_idx = 0; l_idx < letters.len; l_idx++) {
      char *l = letters.data[l_idx];
      for (int b_idx = 0; b_idx < bools.len; b_idx++) {
        int b = bools.data[b_idx];
        tmp4.data[tmp5] = (CombosItem){.n = n, .l = l, .b = b};
        tmp5++;
      }
    }
  }
  tmp4.len = tmp5;
  list_CombosItem combos = tmp4;
  printf("%s\n", "--- Cross Join of three lists ---");
  for (int tmp6 = 0; tmp6 < combos.len; tmp6++) {
    combosItem c = combos.data[tmp6];
    printf("%d ", c.n);
    printf("%s ", c.l);
    printf("%s\n", (c.b) ? "true" : "false");
  }
  return 0;
}
