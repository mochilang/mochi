#include <iostream>

int add(int a, int b) { return (a + b); }

int main() {
  auto add5 = [=](auto p1) { return add(5, p1); };
  std::cout << std::boolalpha << add5(3) << std::endl;
  return 0;
}
