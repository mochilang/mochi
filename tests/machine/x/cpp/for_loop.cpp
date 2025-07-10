#include <iostream>

int main() {
  for (int i = 1; i < 4; ++i) {
    {
      std::cout << std::boolalpha << i;
      std::cout << std::endl;
    }
  }
  return 0;
}
