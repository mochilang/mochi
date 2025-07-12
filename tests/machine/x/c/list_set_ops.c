#include <stdio.h>
#include <stdlib.h>

int main() {
  int tmp1[] = {1, 2, 3};
  for (int i2 = 0; i2 < 3; i2++) {
    if (i2 > 0)
      printf(" ");
    printf("%d", tmp1[i2]);
  }
  printf("\n");
  int tmp3[] = {1, 3};
  for (int i4 = 0; i4 < 2; i4++) {
    if (i4 > 0)
      printf(" ");
    printf("%d", tmp3[i4]);
  }
  printf("\n");
  int tmp5[] = {2};
  for (int i6 = 0; i6 < 1; i6++) {
    if (i6 > 0)
      printf(" ");
    printf("%d", tmp5[i6]);
  }
  printf("\n");
  printf("%d\n", 4);
  return 0;
}
