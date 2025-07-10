#include <iostream>
#include <vector>

std::vector<int> nums = std::vector<decltype(1)>{1, 2, 3};
auto result = ([]() {
  int __sum = 0;
  for (auto n : nums) {
    if (!((n > 1)))
      continue;
    __sum += n;
  }
  return __sum;
})();

int main() {
  std::cout << std::boolalpha << result << std::endl;
  return 0;
}
