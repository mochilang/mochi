#include <stdio.h>
#include <stdlib.h>

int main() {
  int x = 8;
  char *msg = (x > 10 ? "big" : (x > 5 ? "medium" : "small"));
  printf("%s\n", msg);
  return 0;
}
