#include <stdio.h>
#include <stdlib.h>

static int i = 0;

int main() {
  while (i < 3) {
    printf("%d\n", i);
    i = i + 1;
  }
  return 0;
}
