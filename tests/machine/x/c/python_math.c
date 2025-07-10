#include <stdio.h>
#include <stdlib.h>

extern double math_pi;
extern double math_e;
extern double math_sqrt(double x);
extern double math_pow(double x, double y);
extern double math_sin(double x);
extern double math_log(double x);

static double r = 3;

int main() {
  __auto_type area = math.pi * math.pow(r, 2.0);
  __auto_type root = math.sqrt(49.0);
  __auto_type sin45 = math.sin(math.pi / 4.0);
  __auto_type log_e = math.log(math.e);
  printf("%s ", "Circle area with r =");
  printf("%d ", r);
  printf("%s ", "=>");
  printf("%d\n", area);
  printf("%s ", "Square root of 49:");
  printf("%.17g\n", root);
  printf("%s ", "sin(π/4):");
  printf("%.17g\n", sin45);
  printf("%s ", "log(e):");
  printf("%.17g\n", log_e);
  return 0;
}
