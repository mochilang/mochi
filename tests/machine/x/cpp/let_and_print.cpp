#include <iostream>

auto a = 10;
int b = 20;

int main() {
  {
    std::cout << std::boolalpha << (a + b);
    std::cout << std::endl;
  }
  return 0;
}
