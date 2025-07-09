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
char *classify(int n) { return (n == 0 ? "zero" : (n == 1 ? "one" : "many")); }

int main() {
  int x = 2;
  char *label =
      (x == 1 ? "one" : (x == 2 ? "two" : (x == 3 ? "three" : "unknown")));
  printf("%s\n", label);
  char *day = "sun";
  char *mood =
      (day == "mon" ? "tired"
                    : (day == "fri" ? "excited"
                                    : (day == "sun" ? "relaxed" : "normal")));
  printf("%s\n", mood);
  int ok = 1;
  char *status = (ok == 1 ? "confirmed" : (ok == 0 ? "denied" : 0));
  printf("%s\n", status);
  printf("%s\n", classify(0));
  printf("%s\n", classify(5));
  return 0;
}
