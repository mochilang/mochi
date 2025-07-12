#include <stdio.h>
#include <stdlib.h>

int main() {
  int tmp1[] = {1, 2, 3};
  int tmp2 = 0;
  for (int i3 = 0; i3 < 3; i3++) {
    tmp2 += tmp1[i3];
  }
  double tmp4 = tmp2 / (double)3;
  printf("%.16g\n", tmp4);
  return 0;
}
