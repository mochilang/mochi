#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static int contains_string(char *s, char *sub) {
  return strstr(s, sub) != NULL;
}
static char *s = "catch";

int main() {
  printf("%s\n", (contains_string(s, "cat")) ? "true" : "false");
  printf("%s\n", (contains_string(s, "dog")) ? "true" : "false");
  return 0;
}
