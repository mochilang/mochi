#include <iostream>

auto s = std::string("catch");

int main() {
  {
    std::cout << std::boolalpha
              << (s.find(std::string("cat")) != std::string::npos);
    std::cout << std::endl;
  }
  {
    std::cout << std::boolalpha
              << (s.find(std::string("dog")) != std::string::npos);
    std::cout << std::endl;
  }
  return 0;
}
