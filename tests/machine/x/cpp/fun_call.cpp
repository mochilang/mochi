#include <iostream>

auto add(auto a, auto b) { return (a + b); }

int main() {
  {
    std::cout << std::boolalpha << add(2, 3);
    std::cout << std::endl;
  }
  return 0;
}
