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
} PairsItem;
typedef struct {
  int len;
  PairsItem *data;
} list_PairsItem;
static list_PairsItem list_PairsItem_create(int len) {
  list_PairsItem l;
  l.len = len;
  l.data = calloc(len, sizeof(PairsItem));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}
static void _print_list_pairsItem(list_PairsItem v) {
  for (int i = 0; i < v.len; i++) {
    PairsItem s = v.data[i];
    printf("map[");
    printf("n:");
    printf("%d", s.n);
    printf(" ");
    printf("l:");
    printf("%s", s.l);
    printf("]");
    if (i < v.len - 1)
      printf(" ");
  }
}

int main() {
  int tmp1_data[] = {1, 2, 3};
  list_int tmp1 = {3, tmp1_data};
  list_int nums = tmp1;
  char *tmp2_data[] = {"A", "B"};
  list_string tmp2 = {2, tmp2_data};
  list_string letters = tmp2;
  list_PairsItem tmp3 = list_PairsItem_create(nums.len * letters.len);
  int tmp4 = 0;
  for (int n_idx = 0; n_idx < nums.len; n_idx++) {
    int n = nums.data[n_idx];
    for (int l_idx = 0; l_idx < letters.len; l_idx++) {
      char *l = letters.data[l_idx];
      if (!(n % 2 == 0)) {
        continue;
      }
      tmp3.data[tmp4] = (PairsItem){.n = n, .l = l};
      tmp4++;
    }
  }
  tmp3.len = tmp4;
  list_PairsItem pairs = tmp3;
  printf("%s\n", "--- Even pairs ---");
  for (int tmp5 = 0; tmp5 < pairs.len; tmp5++) {
    PairsItem p = pairs.data[tmp5];
    printf("%d ", p.n);
    printf("%s\n", p.l);
  }
  return 0;
}
