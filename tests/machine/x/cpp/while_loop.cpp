#include <iostream>

int main() {
  auto i = 0;
  while ((i < 3)) {
    {
      std::cout << std::boolalpha << i;
      std::cout << std::endl;
    }
    i = (i + 1);
  }
  return 0;
}
