#include <iostream>

namespace testpkg {
inline int Add(int a, int b) { return a + b; }
const double Pi = 3.14;
const int Answer = 42;
} // namespace testpkg
int main() {
  {
    std::cout << std::boolalpha << testpkg::Add(2, 3);
    std::cout << std::endl;
  }
  {
    std::cout << std::boolalpha << testpkg::Pi;
    std::cout << std::endl;
  }
  {
    std::cout << std::boolalpha << testpkg::Answer;
    std::cout << std::endl;
  }
  return 0;
}
