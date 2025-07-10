#include <iostream>
#include <vector>

std::vector<int> nums = std::vector<decltype(1)>{1, 2};

int main() {
  nums[1] = 3;
  {
    std::cout << std::boolalpha << nums[1];
    std::cout << std::endl;
  }
  return 0;
}
