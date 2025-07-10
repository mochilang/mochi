#include <stdio.h>
#include <stdlib.h>

static int x = 12;

int main() {
  char *msg = (x > 10 ? "yes" : "no");
  printf("%s\n", msg);
  return 0;
}
