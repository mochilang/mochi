#include <iostream>

int main() {
  {
    std::cout << std::boolalpha
              << std::string(std::string("mochi")).substr(1, (4) - (1));
    std::cout << std::endl;
  }
  return 0;
}
