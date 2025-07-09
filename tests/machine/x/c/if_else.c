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
int main() {
  int x = 5;
  if ((x > 3)) {
    printf("%s\n", "big");
  } else {
    printf("%s\n", "small");
  }
  return 0;
}
