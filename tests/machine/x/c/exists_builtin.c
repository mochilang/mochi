#include <stdio.h>
#include <stdlib.h>

int main() {
  int data[] = {1, 2};
  int tmp1[] = {1, 2};
  list_int tmp2 = list_int_create(2);
  int tmp3 = 0;
  for (int tmp4 = 0; tmp4 < 2; tmp4++) {
    int x = tmp1[tmp4];
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
