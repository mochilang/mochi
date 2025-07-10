#include <iostream>

bool boom() {
  {
    std::cout << std::boolalpha << std::string("boom");
    std::cout << std::endl;
  }
  return true;
}

int main() {
  {
    std::cout << std::boolalpha << ((((1 < 2)) && ((2 < 3))) && ((3 < 4)));
    std::cout << std::endl;
  }
  {
    std::cout << std::boolalpha << ((((1 < 2)) && ((2 > 3))) && boom());
    std::cout << std::endl;
  }
  {
    std::cout << std::boolalpha
              << (((((1 < 2)) && ((2 < 3))) && ((3 > 4))) && boom());
    std::cout << std::endl;
  }
  return 0;
}
