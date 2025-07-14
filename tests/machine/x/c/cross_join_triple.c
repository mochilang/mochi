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
} combo_t;
typedef struct {
  int len;
  combo_t *data;
} combo_list_t;
static combo_list_t combo_list_create(int len) {
  combo_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(combo_t));
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
  combo_list_t tmp4 = combo_list_create(2 * letters.len * 2);
  int tmp5 = 0;
  for (int n_idx = 0; n_idx < 2; n_idx++) {
    int n = tmp2[n_idx];
    for (int l_idx = 0; l_idx < letters.len; l_idx++) {
      char *l = letters.data[l_idx];
      for (int b_idx = 0; b_idx < 2; b_idx++) {
        int b = tmp3[b_idx];
        tmp4.data[tmp5] = (combo_t){.n = n, .l = l, .b = b};
        tmp5++;
      }
    }
  }
  tmp4.len = tmp5;
  combo_list_t combos = tmp4;
  printf("%s\n", "--- Cross Join of three lists ---");
  for (int tmp6 = 0; tmp6 < combos.len; tmp6++) {
    combo_t c = combos.data[tmp6];
    printf("%d %s %s\n", c.n, c.l, (c.b) ? "true" : "false");
  }
  free(combos.data);
  return 0;
}
