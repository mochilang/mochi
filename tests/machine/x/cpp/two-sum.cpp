#include <iostream>
#include <vector>

auto twoSum(auto nums, int target) {
  auto n = nums.size();
  for (int i = 0; i < n; ++i) {
    for (int j = (i + 1); j < n; ++j) {
      if (((nums[i] + nums[j]) == target)) {
        return std::vector<decltype(i)>{i, j};
      }
    }
  }
  return std::vector<decltype((-1))>{(-1), (-1)};
}

int main() {
  auto result = twoSum(std::vector<decltype(2)>{2, 7, 11, 15}, 9);
  std::cout << std::boolalpha << result[0] << std::endl;
  std::cout << std::boolalpha << result[1] << std::endl;
  return 0;
}
