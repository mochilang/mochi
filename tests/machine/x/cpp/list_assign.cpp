#include <algorithm>
#include <iostream>
#include <map>
#include <numeric>
#include <unordered_map>
#include <utility>
#include <vector>

int main() {
  std::vector<int> nums = std::vector<decltype(1)>{1, 2};
  nums[1] = 3;
  {
    std::cout << std::boolalpha << nums[1];
    std::cout << std::endl;
  }
  return 0;
}
