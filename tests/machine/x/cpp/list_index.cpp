#include <iostream>
#include <vector>

int main() {
  std::vector<int> xs = std::vector<decltype(10)>{10, 20, 30};
  {
    std::cout << std::boolalpha << xs[1];
    std::cout << std::endl;
  }
  return 0;
}
