#include <algorithm>
#include <iostream>
#include <map>
#include <numeric>
#include <unordered_map>
#include <utility>
#include <vector>

int main() {
  auto xs = std::vector<decltype(10)>{10, 20, 30};
  {
    std::cout << std::boolalpha << xs[1];
    std::cout << std::endl;
  }
  return 0;
}
