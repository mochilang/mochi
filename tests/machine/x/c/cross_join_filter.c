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
} pair_t;
typedef struct {
  int len;
  pair_t *data;
} pair_list_t;
static pair_list_t pair_list_create(int len) {
  pair_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(pair_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

int main() {
  int nums[] = {1, 2, 3};
  char *tmp1_data[] = {"A", "B"};
  list_string tmp1 = {2, tmp1_data};
  list_string letters = tmp1;
  int tmp2[] = {1, 2, 3};
  pair_list_t tmp3 = pair_list_create(3 * letters.len);
  int tmp4 = 0;
  for (int n_idx = 0; n_idx < 3; n_idx++) {
    int n = tmp2[n_idx];
    for (int l_idx = 0; l_idx < letters.len; l_idx++) {
      char *l = letters.data[l_idx];
      if (!(n % 2 == 0)) {
        continue;
      }
      tmp3.data[tmp4] = (pair_t){.n = n, .l = l};
      tmp4++;
    }
  }
  tmp3.len = tmp4;
  pair_list_t pairs = tmp3;
  printf("%s\n", "--- Even pairs ---");
  for (int tmp5 = 0; tmp5 < pairs.len; tmp5++) {
    pair_t p = pairs.data[tmp5];
    printf("%d %s\n", p.n, p.l);
  }
  free(pairs.data);
  return 0;
}
