#include <stdio.h>
#include <stdlib.h>

int boom(int a, int b) {
  printf("%s\n", "boom");
  return 1;
}

int main() {
  printf("%s\n", (0 && boom(1, 2)) ? "true" : "false");
  printf("%s\n", (1 || boom(1, 2)) ? "true" : "false");
  return 0;
}
