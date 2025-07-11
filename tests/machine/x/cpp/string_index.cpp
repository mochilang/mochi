#include <iostream>
#include <string>

int main() {
  auto s = std::string("mochi");
  std::cout << std::boolalpha << s[1] << std::endl;
  return 0;
}
