#include <stdio.h>
#include <stdlib.h>

static int contains_array_int(int *arr, int len, int item) {
  for (int i = 0; i < len; i++)
    if (arr[i] == item)
      return 1;
  return 0;
}
int main() {
  int xs[] = {1, 2, 3};
  printf("%s\n", (contains_array_int(xs, 3, 2)) ? "true" : "false");
  printf("%s\n", ((!(contains_array_int(xs, 3, 5)))) ? "true" : "false");
  return 0;
}
