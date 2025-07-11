#include <iostream>

int main() {
  auto a = (10 - 3);
  auto b = (2 + 2);
  std::cout << a << std::endl;
  std::cout << std::boolalpha << (a == 7) << std::endl;
  std::cout << std::boolalpha << (b < 5) << std::endl;
  return 0;
}
