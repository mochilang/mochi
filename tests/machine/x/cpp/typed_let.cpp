#include <iostream>

int y = 0;

int main() {
  {
    std::cout << std::boolalpha << y;
    std::cout << std::endl;
  }
  return 0;
}
