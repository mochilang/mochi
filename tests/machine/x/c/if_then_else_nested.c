#include <stdio.h>
#include <stdlib.h>

static int x = 8;

int main() {
  char *msg = (x > 10 ? "big" : (x > 5 ? "medium" : "small"));
  printf("%s\n", msg);
  return 0;
}
