#include <stdio.h>
#include <stdlib.h>

static int x = 2;
static char *day = "sun";
static int ok = 1;

char *classify(int n) { return (n == 0 ? "zero" : (n == 1 ? "one" : "many")); }

int main() {
  __auto_type label =
      (x == 1 ? "one" : (x == 2 ? "two" : (x == 3 ? "three" : "unknown")));
  printf("%s\n", label);
  __auto_type mood =
      (day == "mon" ? "tired"
                    : (day == "fri" ? "excited"
                                    : (day == "sun" ? "relaxed" : "normal")));
  printf("%d\n", mood);
  __auto_type status = (ok == 1 ? "confirmed" : (ok == 0 ? "denied" : 0));
  printf("%d\n", status);
  printf("%s\n", classify(0));
  printf("%s\n", classify(5));
  return 0;
}
