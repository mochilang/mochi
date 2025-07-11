#include <iostream>
#include <vector>

int main() {
  auto nums = std::vector<int>{1, 2};
  nums[1] = 3;
  std::cout << std::boolalpha << nums[1] << std::endl;
  return 0;
}
