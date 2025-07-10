#include <iostream>

#include <cmath>
struct math_t {
  static double sqrt(double x) { return std::sqrt(x); }
  static double pow(double x, double y) { return std::pow(x, y); }
  static double sin(double x) { return std::sin(x); }
  static double log(double x) { return std::log(x); }
  static constexpr double pi = 3.141592653589793;
  static constexpr double e = 2.718281828459045;
};
math_t math;
int main() {
  {
    std::cout << std::boolalpha << math.sqrt(16);
    std::cout << std::endl;
  }
  {
    std::cout << std::boolalpha << math.pi;
    std::cout << std::endl;
  }
  return 0;
}
