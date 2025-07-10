#include <iostream>

int main() {
  {
    std::cout << std::boolalpha
              << (std::string("hello ") + std::string("world"));
    std::cout << std::endl;
  }
  return 0;
}
