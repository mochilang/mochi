#include <stdio.h>
#include <stdlib.h>

static char *s = "catch";

int main() {
  printf("%d\n", "cat" in s);
  printf("%d\n", "dog" in s);
  return 0;
}
