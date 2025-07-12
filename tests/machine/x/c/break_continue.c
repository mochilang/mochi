#include <stdio.h>
#include <stdlib.h>

int main() {
  int numbers[] = {1, 2, 3, 4, 5, 6, 7, 8, 9};
  int tmp1[] = {1, 2, 3, 4, 5, 6, 7, 8, 9};
  for (int tmp2 = 0; tmp2 < 9; tmp2++) {
    int n = tmp1[tmp2];
    if (n % 2 == 0) {
      continue;
    }
    if (n > 7) {
      break;
    }
    printf("%s ", "odd number:");
    printf("%d\n", n);
  }
  return 0;
}
