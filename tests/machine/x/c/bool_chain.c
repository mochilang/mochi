// Generated by Mochi compiler v0.10.28 on 2006-01-02T15:04:05Z
#include <stdio.h>
#include <stdlib.h>

int boom() {
  printf("boom\n");
  return 1;
}

int _mochi_main() {
  printf("%s\n", ((1 < 2) && (2 < 3) && (3 < 4)) ? "true" : "false");
  printf("%s\n", ((1 < 2) && (2 > 3) && boom()) ? "true" : "false");
  printf("%s\n", ((1 < 2) && (2 < 3) && (3 > 4) && boom()) ? "true" : "false");
  return 0;
}
int main() { return _mochi_main(); }
