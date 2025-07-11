#include <iostream>
#include <string>

bool boom(int a, int b) {
  std::cout << std::string("boom") << std::endl;
  return true;
}

int main() {
  std::cout << std::boolalpha << (false && boom(1, 2)) << std::endl;
  std::cout << std::boolalpha << (true || boom(1, 2)) << std::endl;
  return 0;
}
