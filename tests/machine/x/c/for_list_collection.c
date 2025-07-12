#include <stdio.h>
#include <stdlib.h>

int main() {
  int tmp1[] = {1, 2, 3};
  for (int tmp2 = 0; tmp2 < 3; tmp2++) {
    int n = tmp1[tmp2];
    printf("%d\n", n);
  }
  return 0;
}
