#include <iostream>
#include <string>

int main() {
  std::cout << std::boolalpha << (std::string("hello ") + std::string("world"))
            << std::endl;
  return 0;
}
