#include <iostream>

int main() {
  auto a = (10 - 3);
  auto b = (2 + 2);
  std::cout << a << std::endl;
  std::cout << ((a == 7) ? "true" : "false") << std::endl;
  std::cout << ((b < 5) ? "true" : "false") << std::endl;
  return 0;
}
