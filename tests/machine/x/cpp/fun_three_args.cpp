#include <iostream>

auto sum3(auto a, auto b, auto c) { return ((a + b) + c); }

int main() {
  {
    std::cout << std::boolalpha << sum3(1, 2, 3);
    std::cout << std::endl;
  }
  return 0;
}
