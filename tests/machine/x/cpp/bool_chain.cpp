#include <iostream>

bool boom() {
  std::cout << std::string("boom") << std::endl;
  return true;
}

int main() {
  std::cout << std::boolalpha << ((((1 < 2)) && ((2 < 3))) && ((3 < 4)))
            << std::endl;
  std::cout << std::boolalpha << ((((1 < 2)) && ((2 > 3))) && boom())
            << std::endl;
  std::cout << std::boolalpha
            << (((((1 < 2)) && ((2 < 3))) && ((3 > 4))) && boom()) << std::endl;
  return 0;
}
