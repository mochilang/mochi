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
int boom(int a, int b) {
  printf("%s\n", "boom");
  return 1;
}

int main() {
  printf("%d\n", (0 && boom(1, 2)));
  printf("%d\n", (1 || boom(1, 2)));
  return 0;
}
