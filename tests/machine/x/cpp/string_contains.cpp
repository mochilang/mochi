#include <iostream>

auto s = std::string("catch");

int main() {
  std::cout << std::boolalpha
            << (s.find(std::string("cat")) != std::string::npos) << std::endl;
  std::cout << std::boolalpha
            << (s.find(std::string("dog")) != std::string::npos) << std::endl;
  return 0;
}
