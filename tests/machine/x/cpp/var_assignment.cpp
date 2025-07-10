#include <iostream>

auto x = 1;

int main() {
  x = 2;
  {
    std::cout << std::boolalpha << x;
    std::cout << std::endl;
  }
  return 0;
}
