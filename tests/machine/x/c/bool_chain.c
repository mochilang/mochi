#include <stdio.h>
#include <stdlib.h>

typedef struct {
  int len;
  int *data;
} list_int;
static list_int list_int_create(int len) {
  list_int l;
  l.len = len;
  l.data = (int *)malloc(sizeof(int) * len);
  return l;
}
int boom() {
  printf("%s\n", "boom");
  return 1;
}

int main() {
  printf("%s\n", (((((1 < 2)) && ((2 < 3))) && ((3 < 4)))) ? "true" : "false");
  printf("%s\n", (((((1 < 2)) && ((2 > 3))) && boom())) ? "true" : "false");
  printf("%s\n", ((((((1 < 2)) && ((2 < 3))) && ((3 > 4))) && boom()))
                     ? "true"
                     : "false");
  return 0;
}
