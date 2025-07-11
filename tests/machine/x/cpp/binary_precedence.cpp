#include <iostream>

int main() {
  std::cout << (1 + (2 * 3)) << std::endl;
  std::cout << std::boolalpha << (((1 + 2)) * 3) << std::endl;
  std::cout << std::boolalpha << ((2 * 3) + 1) << std::endl;
  std::cout << (2 * ((3 + 1))) << std::endl;
  return 0;
}
