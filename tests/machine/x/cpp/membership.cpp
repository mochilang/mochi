#include <algorithm>
#include <iostream>
#include <vector>

std::vector<int> nums = std::vector<decltype(1)>{1, 2, 3};

int main() {
  std::cout << std::boolalpha
            << (std::find(nums.begin(), nums.end(), 2) != nums.end())
            << std::endl;
  std::cout << std::boolalpha
            << (std::find(nums.begin(), nums.end(), 4) != nums.end())
            << std::endl;
  return 0;
}
