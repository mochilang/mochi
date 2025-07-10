#include <iostream>

auto x = 12;
auto msg = ((x > 10) ? std::string("yes") : std::string("no"));

int main() {
  {
    std::cout << std::boolalpha << msg;
    std::cout << std::endl;
  }
  return 0;
}
