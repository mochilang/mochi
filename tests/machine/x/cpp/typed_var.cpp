#include <iostream>

int x = 0;

int main() {
  {
    std::cout << std::boolalpha << x;
    std::cout << std::endl;
  }
  return 0;
}
