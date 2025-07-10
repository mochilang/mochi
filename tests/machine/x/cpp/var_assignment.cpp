#include <iostream>

int main() {
  auto x = 1;
  x = 2;
  {
    std::cout << std::boolalpha << x;
    std::cout << std::endl;
  }
  return 0;
}
