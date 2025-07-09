#include <stdio.h>
#include <stdlib.h>

int boom() {
  printf("%s\n", "boom");
  return 1;
}

int main() {
  printf("%s\n", ((1 < 2) && (2 < 3) && (3 < 4)) ? "true" : "false");
  printf("%s\n", ((1 < 2) && (2 > 3) && boom()) ? "true" : "false");
  printf("%s\n", ((1 < 2) && (2 < 3) && (3 > 4) && boom()) ? "true" : "false");
  return 0;
}
