#include <stdio.h>
#include <stdlib.h>

static int x = 2;

int main() {
  __auto_type label =
      (x == 1 ? "one" : (x == 2 ? "two" : (x == 3 ? "three" : "unknown")));
  printf("%s\n", label);
  return 0;
}
