#include <algorithm>
#include <iostream>
#include <vector>

std::vector<int> nums = std::vector<decltype(3)>{3, 1, 4};

int main() {
  std::cout << std::boolalpha << (*std::min_element(nums.begin(), nums.end()))
            << std::endl;
  std::cout << std::boolalpha << (*std::max_element(nums.begin(), nums.end()))
            << std::endl;
  return 0;
}
