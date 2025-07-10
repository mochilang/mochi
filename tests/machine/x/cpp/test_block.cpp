#include <iostream>

int main() {
  // test addition works
  auto x = (1 + 2);
  {
    std::cout << std::boolalpha << std::string("ok");
    std::cout << std::endl;
  }
  return 0;
}
