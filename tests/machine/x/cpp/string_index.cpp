#include <iostream>

int main() {
  auto s = std::string("mochi");
  {
    std::cout << std::boolalpha << s[1];
    std::cout << std::endl;
  }
  return 0;
}
