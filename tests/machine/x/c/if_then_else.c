#include <stdio.h>
#include <stdlib.h>

int main() {
  int x = 12;
  char *msg = (x > 10 ? "yes" : "no");
  printf("%s\n", msg);
  return 0;
}
