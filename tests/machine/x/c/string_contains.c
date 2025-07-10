#include <stdio.h>
#include <stdlib.h>

static char *s = "catch";

int main() {
  printf("%d\n", s.contains("cat"));
  printf("%d\n", s.contains("dog"));
  return 0;
}
