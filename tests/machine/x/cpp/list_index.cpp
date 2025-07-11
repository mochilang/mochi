#include <iostream>
#include <vector>

int main() {
  std::vector<int> xs = std::vector<int>{10, 20, 30};
  std::cout << std::boolalpha << xs[1] << std::endl;
  return 0;
}
