#include <iostream>

int main() {
  auto x = 8;
  auto msg =
      ((x > 10) ? std::string("big")
                : ((x > 5) ? std::string("medium") : std::string("small")));
  {
    std::cout << std::boolalpha << msg;
    std::cout << std::endl;
  }
  return 0;
}
