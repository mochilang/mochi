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
int main() {
  int tmp1_data[] = {1, 2};
  list_int tmp1 = {2, tmp1_data};
  list_int data = tmp1;
  list_int tmp2 = list_int_create(data.len);
  int tmp3 = 0;
  for (int tmp4 = 0; tmp4 < data.len; tmp4++) {
    int x = data.data[tmp4];
    if (!(x == 1)) {
      continue;
    }
    tmp2.data[tmp3] = x;
    tmp3++;
  }
  tmp2.len = tmp3;
  int flag = tmp2.len > 0;
  printf("%s\n", (flag) ? "true" : "false");
  return 0;
}
