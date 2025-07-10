#include <stdio.h>
#include <stdlib.h>

static int x = 5;

int main() {
  if (x > 3) {
    printf("%s\n", "big");
  } else {
    printf("%s\n", "small");
  }
  return 0;
}
