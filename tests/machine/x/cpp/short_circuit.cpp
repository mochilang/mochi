#include <iostream>

auto boom(auto a, auto b) {
  {
    std::cout << std::boolalpha << std::string("boom");
    std::cout << std::endl;
  }
  return true;
}

int main() {
  {
    std::cout << std::boolalpha << (false && boom(1, 2));
    std::cout << std::endl;
  }
  {
    std::cout << std::boolalpha << (true || boom(1, 2));
    std::cout << std::endl;
  }
  return 0;
}
