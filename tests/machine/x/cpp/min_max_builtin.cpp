#include <algorithm>
#include <iostream>
#include <vector>

int main() {
  std::vector<int> nums = std::vector<int>{3, 1, 4};
  std::cout << std::boolalpha << (*std::min_element(nums.begin(), nums.end()))
            << std::endl;
  std::cout << std::boolalpha << (*std::max_element(nums.begin(), nums.end()))
            << std::endl;
  return 0;
}
