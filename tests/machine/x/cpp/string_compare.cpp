#include <iostream>

int main() {
  {
    std::cout << std::boolalpha << (std::string("a") < std::string("b"));
    std::cout << std::endl;
  }
  {
    std::cout << std::boolalpha << (std::string("a") <= std::string("a"));
    std::cout << std::endl;
  }
  {
    std::cout << std::boolalpha << (std::string("b") > std::string("a"));
    std::cout << std::endl;
  }
  {
    std::cout << std::boolalpha << (std::string("b") >= std::string("b"));
    std::cout << std::endl;
  }
  return 0;
}
