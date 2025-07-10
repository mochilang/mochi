#include <stdio.h>
#include <stdlib.h>

int main() {
  __auto_type a = 10 - 3;
  __auto_type b = 2 + 2;
  printf("%d\n", a);
  printf("%s\n", (a == 7) ? "true" : "false");
  printf("%s\n", (b < 5) ? "true" : "false");
  return 0;
}
