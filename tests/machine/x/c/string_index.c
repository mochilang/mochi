#include <stdio.h>
#include <stdlib.h>

static char *s = "mochi";

int main() {
  printf("%.16g\n", s.data[1]);
  return 0;
}
