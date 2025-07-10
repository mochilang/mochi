#include <iostream>

int main() {
  {
    std::cout << std::boolalpha << (1 + (2 * 3));
    std::cout << std::endl;
  }
  {
    std::cout << std::boolalpha << (((1 + 2)) * 3);
    std::cout << std::endl;
  }
  {
    std::cout << std::boolalpha << ((2 * 3) + 1);
    std::cout << std::endl;
  }
  {
    std::cout << std::boolalpha << (2 * ((3 + 1)));
    std::cout << std::endl;
  }
  return 0;
}
