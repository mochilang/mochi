#include <stdio.h>
#include <stdlib.h>

static void test_addition_works() {
  int x = 1 + 2;
  if (!(x == 3)) {
    fprintf(stderr, "expect failed\n");
    exit(1);
  }
}

int main() {
  printf("%s\n", "ok");
  test_addition_works();
  return 0;
}
