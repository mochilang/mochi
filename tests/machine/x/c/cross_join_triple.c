#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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
  int nums[] = {1, 2};
  char *tmp1_data[] = {"A", "B"};
  list_string tmp1 = {2, tmp1_data};
  list_string letters = tmp1;
  int bools[] = {1, 0};
  int tmp2[] = {1, 2};
  int tmp3[] = {1, 0};
  list_CombosItem tmp4 = list_CombosItem_create(2 * letters.len * 2);
  int tmp5 = 0;
  for (int n_idx = 0; n_idx < 2; n_idx++) {
    int n = tmp2[n_idx];
    for (int l_idx = 0; l_idx < letters.len; l_idx++) {
      char *l = letters.data[l_idx];
      for (int b_idx = 0; b_idx < 2; b_idx++) {
        int b = tmp3[b_idx];
        tmp4.data[tmp5] = (CombosItem){.n = n, .l = l, .b = b};
        tmp5++;
      }
    }
  }
  tmp4.len = tmp5;
  list_CombosItem combos = tmp4;
  printf("%s\n", "--- Cross Join of three lists ---");
  // unsupported dynamic list iteration
  for (;;) {
    break;
  }
  return 0;
}
