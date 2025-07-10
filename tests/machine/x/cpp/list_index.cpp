#include <iostream>
#include <vector>

std::vector<int> xs = std::vector<decltype(10)>{10, 20, 30};

int main() {
  std::cout << std::boolalpha << xs[1] << std::endl;
  return 0;
}
