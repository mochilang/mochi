#include <iostream>

auto k = 2;

int inc(int x) { return (x + k); }

int main() {
  {
    std::cout << std::boolalpha << inc(3);
    std::cout << std::endl;
  }
  return 0;
}
