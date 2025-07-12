#include <stdio.h>
#include <stdlib.h>

int main() {
  int a[] = {1, 2};
  int tmp1[] = {1, 2, 3};
  for (int i2 = 0; i2 < 3; i2++) {
    if (i2 > 0)
      printf(" ");
    printf("%d", tmp1[i2]);
  }
  printf("\n");
  return 0;
}
