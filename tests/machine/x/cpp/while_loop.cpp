#include <iostream>

auto i = 0;

int main() {
  while ((i < 3)) {
    {
      std::cout << std::boolalpha << i;
      std::cout << std::endl;
    }
    i = (i + 1);
  }
  return 0;
}
