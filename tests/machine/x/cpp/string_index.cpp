#include <iostream>

auto s = std::string("mochi");

int main() {
  {
    std::cout << std::boolalpha << s[1];
    std::cout << std::endl;
  }
  return 0;
}
