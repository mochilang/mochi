#include <algorithm>
#include <iostream>
#include <map>
#include <numeric>
#include <unordered_map>
#include <utility>
#include <vector>

int main() {
  auto nums = std::vector<decltype(3)>{3, 1, 4};
  {
    std::cout << std::boolalpha
              << (*std::min_element(nums.begin(), nums.end()));
    std::cout << std::endl;
  }
  {
    std::cout << std::boolalpha
              << (*std::max_element(nums.begin(), nums.end()));
    std::cout << std::endl;
  }
  return 0;
}
