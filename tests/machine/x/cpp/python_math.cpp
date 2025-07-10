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
  auto r = 3;
  auto area = (math::pi * math::pow(r, 2));
  auto root = math::sqrt(49);
  auto sin45 = math::sin((math::pi / 4));
  auto log_e = math::log(math::e);
  {
    std::cout << std::boolalpha << std::string("Circle area with r =");
    std::cout << ' ';
    std::cout << std::boolalpha << r;
    std::cout << ' ';
    std::cout << std::boolalpha << std::string("=>");
    std::cout << ' ';
    std::cout << std::boolalpha << area;
    std::cout << std::endl;
  }
  {
    std::cout << std::boolalpha << std::string("Square root of 49:");
    std::cout << ' ';
    std::cout << std::boolalpha << root;
    std::cout << std::endl;
  }
  {
    std::cout << std::boolalpha << std::string("sin(π/4):");
    std::cout << ' ';
    std::cout << std::boolalpha << sin45;
    std::cout << std::endl;
  }
  {
    std::cout << std::boolalpha << std::string("log(e):");
    std::cout << ' ';
    std::cout << std::boolalpha << log_e;
    std::cout << std::endl;
  }
  return 0;
}
