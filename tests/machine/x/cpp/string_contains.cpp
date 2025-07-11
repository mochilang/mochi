#include <iostream>
#include <string>

int main() {
  auto s = std::string("catch");
  std::cout << std::boolalpha
            << (s.find(std::string("cat")) != std::string::npos) << std::endl;
  std::cout << std::boolalpha
            << (s.find(std::string("dog")) != std::string::npos) << std::endl;
  return 0;
}
