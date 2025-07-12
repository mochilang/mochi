#include <iostream>
#include <string>

bool boom() {
  std::cout << std::string("boom") << std::endl;
  return true;
}

int main() {
  std::cout << (((((1 < 2)) && ((2 < 3))) && ((3 < 4))) ? "true" : "false")
            << std::endl;
  std::cout << (((((1 < 2)) && ((2 > 3))) && boom()) ? "true" : "false")
            << std::endl;
  std::cout << ((((((1 < 2)) && ((2 < 3))) && ((3 > 4))) && boom()) ? "true"
                                                                    : "false")
            << std::endl;
  return 0;
}
