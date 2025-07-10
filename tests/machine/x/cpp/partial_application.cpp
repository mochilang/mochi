#include <iostream>

auto add(auto a, auto b) { return (a + b); }

int main() {
  auto add5 = [=](auto p1) { return add(5, p1); };
  {
    std::cout << std::boolalpha << add5(3);
    std::cout << std::endl;
  }
  return 0;
}
