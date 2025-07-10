#include <iostream>
#include <vector>

int main() {
  {
    std::cout << std::boolalpha << std::vector<decltype(1)>{1, 2, 3}.size();
    std::cout << std::endl;
  }
  return 0;
}
