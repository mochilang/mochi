#include <algorithm>
#include <iostream>
#include <map>
#include <numeric>
#include <unordered_map>
#include <utility>
#include <vector>

int main() {
  std::vector<int> xs = std::vector<decltype(1)>{1, 2, 3};
  {
    std::cout << std::boolalpha
              << (std::find(xs.begin(), xs.end(), 2) != xs.end());
    std::cout << std::endl;
  }
  {
    std::cout << std::boolalpha
              << (!((std::find(xs.begin(), xs.end(), 5) != xs.end())));
    std::cout << std::endl;
  }
  return 0;
}
