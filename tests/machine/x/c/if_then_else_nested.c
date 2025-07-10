#include <stdio.h>
#include <stdlib.h>

static int x = 8;

int main() {
  __auto_type msg = (x > 10 ? "big" : (x > 5 ? "medium" : "small"));
  printf("%s\n", msg);
  return 0;
}
