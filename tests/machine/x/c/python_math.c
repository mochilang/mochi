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
  double area = 3.141592653589793 * (r * r);
  double root = 7;
  double sin45 = __builtin_sin(3.141592653589793 / 4.0);
  double log_e = __builtin_log(2.718281828459045);
  printf("%s ", "Circle area with r =");
  printf("%.16g ", r);
  printf("%s ", "=>");
  printf("%.16g\n", area);
  printf("%s ", "Square root of 49:");
  printf("%.16g\n", root);
  printf("%s ", "sin(π/4):");
  printf("%.16g\n", sin45);
  printf("%s ", "log(e):");
  printf("%.16g\n", log_e);
  return 0;
}
