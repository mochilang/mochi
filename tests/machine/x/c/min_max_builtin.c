#include <stdio.h>
#include <stdlib.h>

int main() {
  int nums[] = {3, 1, 4};
  int tmp1[] = {3, 1, 4};
  int tmp2 = tmp1[0];
  for (int i3 = 1; i3 < 3; i3++) {
    if (tmp1[i3] < tmp2)
      tmp2 = tmp1[i3];
  }
  printf("%d\n", tmp2);
  int tmp4[] = {3, 1, 4};
  int tmp5 = tmp4[0];
  for (int i6 = 1; i6 < 3; i6++) {
    if (tmp4[i6] > tmp5)
      tmp5 = tmp4[i6];
  }
  printf("%d\n", tmp5);
  return 0;
}
