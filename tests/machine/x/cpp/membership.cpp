#include <algorithm>
#include <iostream>
#include <vector>

int main() {
  auto nums = std::vector<int>{1, 2, 3};
  std::cout << std::boolalpha
            << (std::find(nums.begin(), nums.end(), 2) != nums.end())
            << std::endl;
  std::cout << std::boolalpha
            << (std::find(nums.begin(), nums.end(), 4) != nums.end())
            << std::endl;
  return 0;
}
