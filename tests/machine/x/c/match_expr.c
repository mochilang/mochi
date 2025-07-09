#include <stdio.h>
#include <stdlib.h>

int main() {
  int x = 2;
  char *label =
      (x == 1 ? "one" : (x == 2 ? "two" : (x == 3 ? "three" : "unknown")));
  printf("%s\n", label);
  return 0;
}
