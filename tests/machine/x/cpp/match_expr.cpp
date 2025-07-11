#include <iostream>
#include <string>

int main() {
  auto x = 2;
  auto label = ([&]() {
    auto __v = x;
    if (__v == 1)
      return std::string("one");
    else if (__v == 2)
      return std::string("two");
    else if (__v == 3)
      return std::string("three");
    return std::string("unknown");
  })();
  std::cout << std::boolalpha << label << std::endl;
  return 0;
}
