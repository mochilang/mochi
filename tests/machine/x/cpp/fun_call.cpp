#include <iostream>

int add(int a, int b) { return (a + b); }

int main() {
  std::cout << std::boolalpha << add(2, 3) << std::endl;
  return 0;
}
