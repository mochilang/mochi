// Generated by Mochi compiler v0.10.28 on 2006-01-02T15:04:05Z
#include <stdio.h>
#include <stdlib.h>

static int x = 2;
static char *day = "sun";
static int ok = 1;

char *classify(int n) { return (n == 0 ? "zero" : (n == 1 ? "one" : "many")); }

int _mochi_main() {
  char *label =
      (x == 1 ? "one" : (x == 2 ? "two" : (x == 3 ? "three" : "unknown")));
  printf("%s\n", label);
  char *mood =
      (day == "mon" ? "tired"
                    : (day == "fri" ? "excited"
                                    : (day == "sun" ? "relaxed" : "normal")));
  printf("%s\n", mood);
  char *status = (ok == 1 ? "confirmed" : (ok == 0 ? "denied" : NULL));
  printf("%s\n", status);
  printf("%s\n", classify(0));
  printf("%s\n", classify(5));
  return 0;
}
int main() { return _mochi_main(); }
