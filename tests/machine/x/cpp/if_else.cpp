#include <iostream>

auto x = 5;

int main() {
  if ((x > 3)) {
    {
      std::cout << std::boolalpha << std::string("big");
      std::cout << std::endl;
    }
  } else {
    {
      std::cout << std::boolalpha << std::string("small");
      std::cout << std::endl;
    }
  }
  return 0;
}
