#include <iostream>

auto x = 8;
auto msg =
    ((x > 10) ? std::string("big")
              : ((x > 5) ? std::string("medium") : std::string("small")));

int main() {
  std::cout << std::boolalpha << msg << std::endl;
  return 0;
}
