#include <iostream>

auto r = 3;
auto area = (math.pi * math.pow(r, 2));
auto root = math.sqrt(49);
auto sin45 = math.sin((math.pi / 4));
auto log_e = math.log(math.e);

int main() {
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
