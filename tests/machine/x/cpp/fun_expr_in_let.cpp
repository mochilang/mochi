#include <iostream>

int main() {
  auto square = [=](int x) { return (x * x); };
  std::cout << std::boolalpha << square(6) << std::endl;
  return 0;
}
