#include <iostream>

auto square = [=](int x) { return (x * x); };

int main() {
  {
    std::cout << std::boolalpha << square(6);
    std::cout << std::endl;
  }
  return 0;
}
