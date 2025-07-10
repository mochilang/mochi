#include <iostream>

int main() {
  auto a = (10 - 3);
  auto b = (2 + 2);
  {
    std::cout << std::boolalpha << a;
    std::cout << std::endl;
  }
  {
    std::cout << std::boolalpha << (a == 7);
    std::cout << std::endl;
  }
  {
    std::cout << std::boolalpha << (b < 5);
    std::cout << std::endl;
  }
  return 0;
}
