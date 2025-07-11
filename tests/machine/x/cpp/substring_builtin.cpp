#include <iostream>
#include <string>

int main() {
  std::cout << std::string(std::string("mochi")).substr(1, (4) - (1))
            << std::endl;
  return 0;
}
