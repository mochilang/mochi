#include <iostream>

#include <cmath>
namespace math {
inline double sqrt(double x) { return std::sqrt(x); }
inline double pow(double x, double y) { return std::pow(x, y); }
inline double sin(double x) { return std::sin(x); }
inline double log(double x) { return std::log(x); }
const double pi = 3.141592653589793;
const double e = 2.718281828459045;
} // namespace math
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
