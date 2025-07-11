#include <iostream>
#include <string>

int main() {
  auto x = 12;
  auto msg = ((x > 10) ? std::string("yes") : std::string("no"));
  std::cout << std::boolalpha << msg << std::endl;
  return 0;
}
