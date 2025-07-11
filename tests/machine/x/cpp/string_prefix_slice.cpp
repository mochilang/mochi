#include <iostream>
#include <string>

int main() {
  auto prefix = std::string("fore");
  auto s1 = std::string("forest");
  std::cout << std::boolalpha
            << (std::string(s1).substr(0, (prefix.size()) - (0)) == prefix)
            << std::endl;
  auto s2 = std::string("desert");
  std::cout << std::boolalpha
            << (std::string(s2).substr(0, (prefix.size()) - (0)) == prefix)
            << std::endl;
  return 0;
}
