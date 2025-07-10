#include <iostream>

auto prefix = std::string("fore");
auto s1 = std::string("forest");
auto s2 = std::string("desert");

int main() {
  {
    std::cout << std::boolalpha
              << (std::string(s1).substr(0, (prefix.size()) - (0)) == prefix);
    std::cout << std::endl;
  }
  {
    std::cout << std::boolalpha
              << (std::string(s2).substr(0, (prefix.size()) - (0)) == prefix);
    std::cout << std::endl;
  }
  return 0;
}
