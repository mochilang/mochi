#include <iostream>
#include <vector>

int main() {
  for (auto n : std::vector<int>{1, 2, 3}) {
    std::cout << std::boolalpha << n << std::endl;
  }
  return 0;
}
